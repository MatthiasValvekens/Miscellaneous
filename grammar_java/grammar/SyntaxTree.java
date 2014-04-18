package grammar;

import grammar.util.Tree;

class SyntaxTree extends Tree<String>{
	public SyntaxTree(Tree<String> parent, String value) {
		super(value);
	}
	public SyntaxTree(String value){
		super(value);
	}
}
