function skipWhitespace(text, index) {
    while (index < text.length && /\s/.test(text[index])) {
        index += 1;
    }
    return index;
}

function parseJsonString(text, index) {
    if (text[index] !== '"') {
        return null;
    }

    let i = index + 1;
    let escaped = false;
    while (i < text.length) {
        const ch = text[i];
        if (escaped) {
            escaped = false;
        } else if (ch === '\\') {
            escaped = true;
        } else if (ch === '"') {
            const raw = text.slice(index, i + 1);
            return {
                value: JSON.parse(raw),
                end: i + 1
            };
        }
        i += 1;
    }

    return null;
}

function parseNestedValueRange(text, index) {
    const start = skipWhitespace(text, index);
    if (start >= text.length) {
        return null;
    }

    const ch = text[start];

    if (ch === '"') {
        const parsed = parseJsonString(text, start);
        return parsed ? { start: start, end: parsed.end } : null;
    }

    if (ch === '{') {
        let i = start + 1;
        while (i < text.length) {
            i = skipWhitespace(text, i);
            if (i >= text.length) {
                return null;
            }
            if (text[i] === '}') {
                return { start: start, end: i + 1 };
            }
            const keyMatch = parseJsonString(text, i);
            if (!keyMatch) {
                return null;
            }
            i = skipWhitespace(text, keyMatch.end);
            if (text[i] !== ':') {
                return null;
            }
            i = skipWhitespace(text, i + 1);
            const valueRange = parseNestedValueRange(text, i);
            if (!valueRange) {
                return null;
            }
            i = skipWhitespace(text, valueRange.end);
            if (text[i] === '}') {
                return { start: start, end: i + 1 };
            }
            if (text[i] !== ',') {
                return null;
            }
            i = skipWhitespace(text, i + 1);
        }
    }

    if (ch === '[') {
        let i = start + 1;
        while (i < text.length) {
            i = skipWhitespace(text, i);
            if (i >= text.length) {
                return null;
            }
            if (text[i] === ']') {
                return { start: start, end: i + 1 };
            }
            const valueRange = parseNestedValueRange(text, i);
            if (!valueRange) {
                return null;
            }
            i = skipWhitespace(text, valueRange.end);
            if (text[i] === ']') {
                return { start: start, end: i + 1 };
            }
            if (text[i] !== ',') {
                return null;
            }
            i = skipWhitespace(text, i + 1);
        }
    }

    if (text.slice(start, start + 4) === 'true') {
        return { start: start, end: start + 4 };
    }
    if (text.slice(start, start + 5) === 'false') {
        return { start: start, end: start + 5 };
    }
    if (text.slice(start, start + 4) === 'null') {
        return { start: start, end: start + 4 };
    }

    if (text[start] === '-' || /[0-9]/.test(text[start])) {
        let j = start + 1;
        while (j < text.length && /[0-9eE+\-\.]/.test(text[j])) {
            j += 1;
        }
        return { start: start, end: j };
    }

    return null;
}

function findJsonValueRange(text, pointer) {
    if (!text || pointer === null || pointer === undefined) {
        return null;
    }

    try {
        const tokens = pointer
            .replace(/~1/g, '/')
            .replace(/~0/g, '~')
            .split('/')
            .filter(Boolean)
            .map(function (token) {
                return decodeURIComponent(token);
            });

        if (tokens.length === 0) {
            const rootStart = skipWhitespace(text, 0);
            return parseNestedValueRange(text, rootStart);
        }

        function walkRange(sourceText, index, tokenIndex) {
            const start = skipWhitespace(sourceText, index);
            if (start >= sourceText.length) {
                return null;
            }

            const ch = sourceText[start];

            if (ch === '{') {
                let i = start + 1;
                while (i < sourceText.length) {
                    i = skipWhitespace(sourceText, i);
                    if (i >= sourceText.length) {
                        return null;
                    }
                    if (sourceText[i] === '}') {
                        return null;
                    }

                    const keyMatch = parseJsonString(sourceText, i);
                    if (!keyMatch) {
                        return null;
                    }
                    const key = keyMatch.value;
                    i = skipWhitespace(sourceText, keyMatch.end);
                    if (sourceText[i] !== ':') {
                        return null;
                    }
                    i = skipWhitespace(sourceText, i + 1);

                    if (tokenIndex < tokens.length && key === tokens[tokenIndex]) {
                        if (tokenIndex === tokens.length - 1) {
                            const range = parseNestedValueRange(sourceText, i);
                            return range ? range : null;
                        }
                        const nested = walkRange(sourceText, i, tokenIndex + 1);
                        if (nested) {
                            return nested;
                        }
                    }

                    const skipped = parseNestedValueRange(sourceText, i);
                    if (!skipped) {
                        return null;
                    }
                    i = skipWhitespace(sourceText, skipped.end);
                    if (sourceText[i] === '}') {
                        return null;
                    }
                    if (sourceText[i] !== ',') {
                        return null;
                    }
                    i = skipWhitespace(sourceText, i + 1);
                }
            }

            if (ch === '[') {
                let i = start + 1;
                let arrayIndex = 0;
                while (i < sourceText.length) {
                    i = skipWhitespace(sourceText, i);
                    if (i >= sourceText.length) {
                        return null;
                    }
                    if (sourceText[i] === ']') {
                        return null;
                    }

                    const valueRange = parseNestedValueRange(sourceText, i);
                    if (!valueRange) {
                        return null;
                    }

                    const token = tokens[tokenIndex];
                    const isTargetIndex = token !== undefined && /^\d+$/.test(token) && Number(token) === arrayIndex;
                    if (isTargetIndex && tokenIndex === tokens.length - 1) {
                        return valueRange;
                    }
                    if (isTargetIndex && tokenIndex < tokens.length - 1) {
                        const nested = walkRange(sourceText, valueRange.start, tokenIndex + 1);
                        if (nested) {
                            return nested;
                        }
                    }

                    i = skipWhitespace(sourceText, valueRange.end);
                    if (sourceText[i] === ']') {
                        return null;
                    }
                    if (sourceText[i] !== ',') {
                        return null;
                    }
                    i = skipWhitespace(sourceText, i + 1);
                    arrayIndex += 1;
                }
            }

            return null;
        }

        return walkRange(text, 0, 0);
    } catch (error) {
        return null;
    }
}

function findXmlMarkupEnd(text, start) {
    let quote = '';
    for (let index = start + 1; index < text.length; index += 1) {
        const character = text[index];
        if (quote) {
            if (character === quote) {
                quote = '';
            }
        } else if (character === '"' || character === "'") {
            quote = character;
        } else if (character === '>') {
            return index + 1;
        }
    }
    return text.length;
}

function findXmlValueRange(text, path) {
    if (!text || !path || path[0] !== '/') {
        return null;
    }

    const parts = path.slice(1).split('/');
    let special = null;
    if (parts[parts.length - 1] === 'text()') {
        special = 'text';
        parts.pop();
    } else if (parts[parts.length - 1].startsWith('@')) {
        special = parts.pop().slice(1);
    }
    if (parts.length === 0 || !parts[0]) {
        return null;
    }

    let root = null;
    const stack = [];
    let cursor = 0;
    while (cursor < text.length) {
        const tagStart = text.indexOf('<', cursor);
        if (tagStart < 0) {
            break;
        }
        if (stack.length > 0 && stack[stack.length - 1].text === null && tagStart > cursor) {
            stack[stack.length - 1].text = { start: cursor, end: tagStart };
        }
        if (text.startsWith('<!--', tagStart)) {
            const close = text.indexOf('-->', tagStart + 4);
            cursor = close < 0 ? text.length : close + 3;
            continue;
        }
        if (text.startsWith('<![CDATA[', tagStart)) {
            const close = text.indexOf(']]>', tagStart + 9);
            if (stack.length > 0 && stack[stack.length - 1].text === null && close >= 0) {
                stack[stack.length - 1].text = { start: tagStart + 9, end: close };
            }
            cursor = close < 0 ? text.length : close + 3;
            continue;
        }

        const tagEnd = findXmlMarkupEnd(text, tagStart);
        const markup = text.slice(tagStart, tagEnd);
        cursor = tagEnd;
        if (markup.startsWith('<?') || markup.startsWith('<!')) {
            continue;
        }
        if (markup.startsWith('</')) {
            if (stack.length > 0) {
                stack.pop().end = tagEnd;
            }
            continue;
        }

        const nameMatch = /^<([^\s/>]+)/.exec(markup);
        if (!nameMatch) {
            continue;
        }
        const attributes = {};
        let offset = nameMatch[0].length;
        while (offset < markup.length) {
            const attributeMatch = /\s+([^\s=/>]+)\s*=\s*(["'])/.exec(markup.slice(offset));
            if (!attributeMatch) {
                break;
            }
            const attributeName = attributeMatch[1];
            const quote = attributeMatch[2];
            const valueStart = offset + attributeMatch[0].length;
            const valueEnd = markup.indexOf(quote, valueStart);
            if (valueEnd < 0) {
                break;
            }
            attributes[attributeName] = {
                start: tagStart + valueStart - 1,
                end: tagStart + valueEnd + 1
            };
            offset = valueEnd + 1;
        }

        const node = {
            name: nameMatch[1],
            start: tagStart,
            end: tagEnd,
            attributes: attributes,
            text: null,
            children: []
        };
        if (stack.length > 0) {
            stack[stack.length - 1].children.push(node);
        } else {
            root = node;
        }
        if (!/\/\s*>$/.test(markup)) {
            stack.push(node);
        }
    }

    if (!root) {
        return null;
    }
    const parseSegment = function (segment) {
        const match = /^(.*?)(?:\[(\d+)\])?$/.exec(segment);
        return match ? { name: match[1], index: Number(match[2] || 1) - 1 } : null;
    };
    const rootSegment = parseSegment(parts[0]);
    if (!rootSegment || rootSegment.name !== root.name) {
        return null;
    }
    let node = root;
    for (let index = 1; index < parts.length; index += 1) {
        const segment = parseSegment(parts[index]);
        if (!segment) {
            return null;
        }
        const matches = node.children.filter(function (child) { return child.name === segment.name; });
        if (segment.index < 0 || segment.index >= matches.length) {
            return null;
        }
        node = matches[segment.index];
    }

    if (special === 'text') {
        if (!node.text) {
            return null;
        }
        let start = node.text.start;
        let end = node.text.end;
        while (start < end && /\s/.test(text[start])) start += 1;
        while (end > start && /\s/.test(text[end - 1])) end -= 1;
        return { start: start, end: end };
    }
    if (special !== null) {
        return node.attributes[special] || null;
    }
    return { start: node.start, end: node.end };
}

function parseCsvRecords(text) {
    const records = [];
    let row = [];
    let fieldStart = text.charCodeAt(0) === 0xFEFF ? 1 : 0;
    let cursor = fieldStart;
    let inQuotes = false;
    while (cursor < text.length) {
        const character = text[cursor];
        if (character === '"') {
            if (inQuotes && text[cursor + 1] === '"') {
                cursor += 2;
                continue;
            }
            inQuotes = !inQuotes;
        } else if (!inQuotes && character === ',') {
            row.push({ start: fieldStart, end: cursor });
            fieldStart = cursor + 1;
        } else if (!inQuotes && (character === '\r' || character === '\n')) {
            row.push({ start: fieldStart, end: cursor });
            records.push(row);
            row = [];
            if (character === '\r' && text[cursor + 1] === '\n') cursor += 1;
            fieldStart = cursor + 1;
        }
        cursor += 1;
    }
    if (fieldStart < text.length || row.length > 0 || (text.length > 0 && !/[\r\n]$/.test(text))) {
        row.push({ start: fieldStart, end: text.length });
        records.push(row);
    }
    return records;
}

function csvFieldValue(text, range) {
    let value = text.slice(range.start, range.end);
    if (value.length >= 2 && value[0] === '"' && value[value.length - 1] === '"') {
        value = value.slice(1, -1).replace(/""/g, '"');
    }
    return value;
}

function splitCsvPath(path) {
    const parts = [];
    let current = '';
    let escaped = false;
    for (let index = 0; index < path.length; index += 1) {
        const character = path[index];
        if (escaped) {
            current += character;
            escaped = false;
        } else if (character === '\\') {
            escaped = true;
        } else if (character === ',') {
            parts.push(current);
            current = '';
        } else {
            current += character;
        }
    }
    if (escaped) current += '\\';
    parts.push(current);
    return parts;
}

function findCsvValueRange(text, path) {
    if (!text) {
        return null;
    }
    if (path === '') {
        return { start: 0, end: text.length };
    }
    const records = parseCsvRecords(text);
    if (records.length === 0) {
        return null;
    }
    const headers = records[0].map(function (range) { return csvFieldValue(text, range); });
    const parts = splitCsvPath(path);
    if (parts.length === 1) {
        const headerIndex = headers.indexOf(parts[0]);
        if (headerIndex >= 0) {
            return records[0][headerIndex];
        }
        if (/^\d+$/.test(parts[0])) {
            const rowIndex = Number(parts[0]);
            if (rowIndex >= 1 && rowIndex < records.length) {
                return { start: records[rowIndex][0].start, end: records[rowIndex][records[rowIndex].length - 1].end };
            }
        }
        return null;
    }
    if (parts.length === 2 && /^\d+$/.test(parts[0])) {
        const rowIndex = Number(parts[0]);
        const columnIndex = headers.indexOf(parts[1]);
        if (rowIndex >= 1 && rowIndex < records.length && columnIndex >= 0) {
            return records[rowIndex][columnIndex] || null;
        }
    }
    return null;
}

function findSourceValueRange(text, path, format) {
    if (format === 'xml') {
        return findXmlValueRange(text, path);
    }
    if (format === 'csv') {
        return findCsvValueRange(text, path);
    }
    return findJsonValueRange(text, path);
}

function markerClassForOperation(op) {
    if (op === 'add') {
        return 'diff-add';
    }
    if (op === 'remove') {
        return 'diff-remove';
    }
    return 'diff-update';
}

function clearEditorHighlights(editorElement) {
    if (!editorElement || !editorElement._diffMarkerIds) {
        return;
    }

    const editor = $(editorElement).data('aceEditor') || ace.edit(editorElement);
    editorElement._diffMarkerIds.forEach(function (markerId) {
        editor.session.removeMarker(markerId);
    });
    editorElement._diffMarkerIds = [];
}

function applySelectionToEditor(editorId, text, pointer, op, side, format) {
    const editorElement = document.getElementById(editorId);
    if (!editorElement || !text) {
        return;
    }

    const editor = $(editorElement).data('aceEditor') || ace.edit(editorElement);
    clearEditorHighlights(editorElement);
    const range = findSourceValueRange(text, pointer, format);
    if (!range) {
        return;
    }

    const startPos = editor.session.doc.indexToPosition(range.start, 0);
    const endPos = editor.session.doc.indexToPosition(range.end, 0);
    const Range = ace.require('ace/range').Range;
    const selectionRange = new Range(startPos.row, startPos.column, endPos.row, endPos.column);
    const markerClass = markerClassForOperation(op || 'update');

    if (!editorElement._diffMarkerIds) {
        editorElement._diffMarkerIds = [];
    }

    /* This does the colored highlighting */
    const markerId = editor.session.addMarker(selectionRange, 'diff-marker ' + markerClass, 'text');
    editorElement._diffMarkerIds.push(markerId);

    if (side === 'base' || side === 'compare') {
        /* This does the grey highlighting */
        editor.selection.setSelectionRange(new Range(startPos.row, startPos.column, startPos.row, startPos.column), false);
        editor.resize(true);
        editor.scrollToLine(startPos.row, true, true, function () { });
    }
}

Shiny.addCustomMessageHandler('sync-diff-selection', function (message) {
    const op = message.operation || 'update';
    const format = message.format || 'json';

    if (op === 'add') {
        if (message.base_path !== null && message.base_path !== undefined) {
            applySelectionToEditor('base_text', message.base_text, message.base_path, 'update', 'base', format);
        }
        if (message.compare_path !== null && message.compare_path !== undefined) {
            applySelectionToEditor('compare_text', message.compare_text, message.compare_path, op, 'compare', format);
        }
        return;
    }

    if (op === 'remove') {
        if (message.compare_path !== null && message.compare_path !== undefined) {
            applySelectionToEditor('compare_text', message.compare_text, message.compare_path, 'update', 'compare', format);
        }
        if (message.base_path !== null && message.base_path !== undefined) {
            applySelectionToEditor('base_text', message.base_text, message.base_path, op, 'base', format);
        }
        return;
    }

    if (message.base_path || (format === 'csv' && message.base_path === '')) {
        applySelectionToEditor('base_text', message.base_text, message.base_path, op, 'base', format);
    }
    if (message.compare_path || (format === 'csv' && message.compare_path === '')) {
        applySelectionToEditor('compare_text', message.compare_text, message.compare_path, op, 'compare', format);
    }
});
