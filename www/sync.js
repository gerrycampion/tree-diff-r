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

function applySelectionToEditor(editorId, text, pointer, op, side) {
    const editorElement = document.getElementById(editorId);
    if (!editorElement || !text) {
        return;
    }

    const editor = $(editorElement).data('aceEditor') || ace.edit(editorElement);
    const range = findJsonValueRange(text, pointer);
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

    clearEditorHighlights(editorElement);
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

    if (op === 'add') {
        if (message.base_path !== null && message.base_path !== undefined) {
            applySelectionToEditor('base_text', message.base_text, message.base_path, 'update', 'base');
        }
        if (message.compare_path !== null && message.compare_path !== undefined) {
            applySelectionToEditor('compare_text', message.compare_text, message.compare_path, op, 'compare');
        }
        return;
    }

    if (op === 'remove') {
        if (message.compare_path !== null && message.compare_path !== undefined) {
            applySelectionToEditor('compare_text', message.compare_text, message.compare_path, 'update', 'compare');
        }
        if (message.base_path !== null && message.base_path !== undefined) {
            applySelectionToEditor('base_text', message.base_text, message.base_path, op, 'base');
        }
        return;
    }

    if (message.base_path) {
        applySelectionToEditor('base_text', message.base_text, message.base_path, op, 'base');
    }
    if (message.compare_path) {
        applySelectionToEditor('compare_text', message.compare_text, message.compare_path, op, 'compare');
    }
});
