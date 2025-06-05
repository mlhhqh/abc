// extension.ts
import * as vscode from 'vscode';
import * as Parser from 'web-tree-sitter';

let parser: Parser | null = null;
let decorationTypes: Map<string, vscode.TextEditorDecorationType> = new Map();
let isEnabled = true;

// Default colors (customizable via settings)
const defaultColors = {
    keyword: '#3d2a5e',
    variable: '#2a3d5e', 
    string: '#2a5e3d'
};

// Node types (exact matches only)
const nodeTypes = {
    keyword: ['if', 'else', 'for', 'while', 'switch', 'case', 'default', 'defer', 'go',
              'select', 'return', 'break', 'continue', 'fallthrough', 'goto', 'range',
              'func', 'var', 'const', 'type', 'struct', 'interface', 'package', 'import'],
    variable: ['identifier', 'field_identifier', 'parameter_declaration'],
    string: ['interpreted_string_literal', 'raw_string_literal', 'rune_literal']
};

export async function activate(context: vscode.ExtensionContext) {
    // Initialize Tree-sitter
    await Parser.init();
    parser = new Parser();
    
    // Load Go language (you can extend this for other languages)
    try {
        const Go = await Parser.Language.load(vscode.Uri.joinPath(context.extensionUri, 'tree-sitter-go.wasm').fsPath);
        parser.setLanguage(Go);
    } catch (error) {
        console.error('Failed to load Go language for tree-sitter:', error);
        return;
    }

    // Create decoration types
    setupDecorationTypes();

    // Register commands
    const toggleCommand = vscode.commands.registerCommand('tsBgHighlight.toggle', () => {
        isEnabled = !isEnabled;
        if (isEnabled) {
            highlightActiveEditor();
            vscode.window.showInformationMessage('Tree-sitter background highlighting enabled');
        } else {
            clearHighlights();
            vscode.window.showInformationMessage('Tree-sitter background highlighting disabled');
        }
    });

    const refreshCommand = vscode.commands.registerCommand('tsBgHighlight.refresh', () => {
        highlightActiveEditor();
    });

    // Listen for editor changes
    const onDidChangeActiveEditor = vscode.window.onDidChangeActiveTextEditor(() => {
        if (isEnabled) {
            highlightActiveEditor();
        }
    });

    const onDidChangeTextDocument = vscode.workspace.onDidChangeTextDocument((event) => {
        if (isEnabled && event.document === vscode.window.activeTextEditor?.document) {
            // Debounce the highlighting
            setTimeout(() => highlightActiveEditor(), 100);
        }
    });

    // Listen for configuration changes
    const onDidChangeConfiguration = vscode.workspace.onDidChangeConfiguration((event) => {
        if (event.affectsConfiguration('tsBgHighlight.colors')) {
            setupDecorationTypes();
            if (isEnabled) {
                highlightActiveEditor();
            }
        }
    });

    // Initial highlight
    if (isEnabled) {
        highlightActiveEditor();
    }

    // Register disposables
    context.subscriptions.push(
        toggleCommand,
        refreshCommand,
        onDidChangeActiveEditor,
        onDidChangeTextDocument,
        onDidChangeConfiguration
    );
}

function setupDecorationTypes() {
    // Dispose existing decoration types
    decorationTypes.forEach(decoration => decoration.dispose());
    decorationTypes.clear();

    // Get colors from configuration
    const config = vscode.workspace.getConfiguration('tsBgHighlight');
    const colors = {
        keyword: config.get<string>('colors.keyword') || defaultColors.keyword,
        variable: config.get<string>('colors.variable') || defaultColors.variable,
        string: config.get<string>('colors.string') || defaultColors.string
    };

    // Create decoration types
    decorationTypes.set('keyword', vscode.window.createTextEditorDecorationType({
        backgroundColor: colors.keyword,
        rangeBehavior: vscode.DecorationRangeBehavior.ClosedClosed
    }));

    decorationTypes.set('variable', vscode.window.createTextEditorDecorationType({
        backgroundColor: colors.variable,
        rangeBehavior: vscode.DecorationRangeBehavior.ClosedClosed
    }));

    decorationTypes.set('string', vscode.window.createTextEditorDecorationType({
        backgroundColor: colors.string,
        rangeBehavior: vscode.DecorationRangeBehavior.ClosedClosed
    }));
}

function highlightActiveEditor() {
    const editor = vscode.window.activeTextEditor;
    if (!editor || !parser) {
        return;
    }

    // Only highlight Go files (extend for other languages)
    if (editor.document.languageId !== 'go') {
        return;
    }

    // Clear existing highlights
    clearHighlights();

    try {
        // Parse the document
        const text = editor.document.getText();
        const tree = parser.parse(text);

        // Collect ranges for each category
        const ranges: Map<string, vscode.Range[]> = new Map();
        ranges.set('keyword', []);
        ranges.set('variable', []);
        ranges.set('string', []);

        // Traverse the syntax tree
        function traverse(node: Parser.SyntaxNode) {
            const type = node.type;

            // Skip functions (calls and definitions) - no highlighting
            if (type.includes('function') || type.includes('call')) {
                // Traverse children but don't highlight this node
                for (const child of node.children) {
                    traverse(child);
                }
                return;
            }

            // Check for exact matches
            let category: string | null = null;
            for (const [cat, types] of Object.entries(nodeTypes)) {
                if (types.includes(type)) {
                    category = cat;
                    break;
                }
            }

            // Add range if category found
            if (category && ranges.has(category)) {
                const startPos = editor.document.positionAt(node.startIndex);
                const endPos = editor.document.positionAt(node.endIndex);
                const range = new vscode.Range(startPos, endPos);
                ranges.get(category)!.push(range);
            }

            // Traverse children
            for (const child of node.children) {
                traverse(child);
            }
        }

        traverse(tree.rootNode);

        // Apply decorations
        for (const [category, categoryRanges] of ranges) {
            const decorationType = decorationTypes.get(category);
            if (decorationType) {
                editor.setDecorations(decorationType, categoryRanges);
            }
        }

    } catch (error) {
        console.error('Error highlighting syntax:', error);
    }
}

function clearHighlights() {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
        return;
    }

    // Clear all decorations
    decorationTypes.forEach(decoration => {
        editor.setDecorations(decoration, []);
    });
}

export function deactivate() {
    // Dispose decoration types
    decorationTypes.forEach(decoration => decoration.dispose());
    decorationTypes.clear();
}
