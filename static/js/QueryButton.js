
function makeQueryButton(where){
	var queryInput = ace.edit(where.attr('id'));
	queryInput.getSession().setMode("ace/mode/prolog");
	queryInput.getSession().setUseWrapMode(true);
	queryInput.setOptions({
		// editor options
		selectionStyle: "text",
		highlightActiveLine: false,
		highlightSelectedWord: false,
		readOnly: true,
		cursorStyle: "slim",
		behavioursEnabled: false,
		wrapBehavioursEnabled: false,
		autoScrollEditorIntoView: false,
		enableMultiselect: false,
		// renderer options
		highlightGutterLine: false,
		maxLines: 15,
		showGutter: false,
		showLineNumbers: false,
		showFoldWidgets: false,
		printMarginColumn: false,
		showPrintMargin: false
	});
	queryInput.setShowPrintMargin(false);
	queryInput.renderer.setScrollMargin(6, 6, 6, 6);
	queryInput.resize(true);
};
