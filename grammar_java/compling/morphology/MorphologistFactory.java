package compling.morphology;

import java.util.HashMap;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import compling.grammar.ContextFreeGrammar;
import compling.grammar.Rule;
import compling.grammar.util.GrammarUtils;
import compling.grammar.util.Pair;
import compling.grammar.util.SyntaxForest;
import compling.grammar.util.Tree;
import compling.parsers.Parser;
import compling.parsers.Tokenizer;

public class MorphologistFactory {
	public static final class MetaMorphologist extends Morphologist {
		
		private static Map<String,String> specialCharacters=specials();
		private static Map<String,String> specials(){
			HashMap<String,String> m=new HashMap<String,String>();
			m.put("\n","Newline");
			m.put("[","LeftBracket");
			m.put("]","RightBracket");
			m.put("->","RuleSep");
			return m;
		}
		private static HashMap<String,Set<String>> standardWordInfo(){
			HashMap<String,Set<String>> res =new HashMap<String,Set<String>>();
			HashSet<String> tokentypedomain=new HashSet<String>(Arrays.asList("String","Newline","LeftBracket","RightBracket","RuleSep"));
			res.put("TokenType", tokentypedomain);
			return res;
		}
		public MetaMorphologist() {
			//The metamorphologist is so simple it doesn't even need a metatransducer! /s
			super(null, standardWordInfo());
		}

		@Override
		public WordInfo deinflect(String token) {
			Map<String,String> info=new HashMap<String,String>();
			if(Tokenizer.alphamatcher.matcher(token).matches()){
				info.put("TokenType", "String");
			}
			else if(specialCharacters.containsKey(token)){
				info.put("TokenType", specialCharacters.get(token));
			} else throw new IllegalArgumentException("Could not read token "+token);
			return new WordInfo(this,token,info);
		}

		@Override
		protected String getSyntacticIdentifier(Map<String, String> info) {
			return info.get("TokenType");
		}

	}

	private static class MorphologistParser extends compling.parsers.Parser{
		public MorphologistParser(Set<String> terminals){
			super(Tokenizer.getGrammarTokenizer(),getMorphologistMetasyntax(),new MetaMorphologist());
		}
		private boolean isSeparatorID(String s){//see, this is a job for the metamorphologist hierarchy
			return false;
		}
		@Override
		protected SyntaxForest postProcess(SyntaxForest f){
			if(f==null || f.size()==0) throw new IllegalArgumentException("Morphology specification is ungrammatical (according to the morphology spec metagrammar).");
			if(f.size()>1) throw new IllegalArgumentException("Morphology specification is ambiguous. (degree "+f.size()+")");			
			return f;
		}
		

	}

	public static ContextFreeGrammar getMorphologistMetasyntax(){
		// TODO : hierarchy of fields
		Set<Rule> metarules= new HashSet<Rule>();
		metarules.add(Rule.fromString("Morphologist->MorphoTokenList Newlines Specification"));
		metarules.add(Rule.fromString("Specification->WordInfoSpec Ruleset"));
		metarules.add(Rule.fromString("MorphoTokenList->LeftBracket MorphoTokens RightBracket"));
		metarules.add(Rule.fromString("MorphoTokens->MorphoToken"));
		metarules.add(Rule.fromString("MorphoTokens->MorphoToken MorphoTokens"));
		metarules.add(Rule.fromString("WordInfoSpec->WordInfoEntry Newlines"));
		metarules.add(Rule.fromString("WordInfoSpec->WordInfoEntry Newlines WordInfoSpec"));
		metarules.add(Rule.fromString("WordInfoEntry->String LabelBody"));
		metarules.add(Rule.fromString("LabelBody->LeftBracket LabelDomain RightBracket"));
		metarules.add(Rule.fromString("LabelDomain->DomainElem LabelDomain"));
		metarules.add(Rule.fromString("LabelDomain->DomainElem"));
		metarules.add(Rule.fromString("Ruleset->Rule Newlines"));
		metarules.add(Rule.fromString("Ruleset->Rule Newlines Ruleset"));
		metarules.add(Rule.fromString("Rule->RuleInput RuleSep RuleOutput"));
		metarules.add(Rule.fromString("RuleInput->RuleSymbol RuleInput"));
		metarules.add(Rule.fromString("RuleOutput->RuleOutput RuleSymbol"));
		metarules.add(Rule.fromString("RuleInput->RuleSymbol"));
		metarules.add(Rule.fromString("RuleOutput->RuleSymbol"));
		metarules.add(Rule.fromString("Newlines->Newline Newlines"));
		metarules.add(Rule.fromString("Newlines->Newline"));
		
		//these hierarchical rules should eventually be handled by the morphologists themselves
		metarules.add(Rule.fromString("DomainElem->String"));
		metarules.add(Rule.fromString("RuleSymbol->String"));
		metarules.add(Rule.fromString("MorphoToken->String"));
		Set<String> nonterminals=GrammarUtils.extractNonterminals(metarules);
		return ContextFreeGrammar.createContextFreeGrammar(new HashSet<String>(), nonterminals, metarules, "Morphologist");
	}
	
	public static Morphologist trainMorphologist(Set<String> alphabet, String spec){
		// TODO : implement this by referring to the metamorphologist's metahierarchy!
		// TODO : as soon as coordinate clauses are available as a metastructure/postprocessor, try to apply them here
		Parser p = new MorphologistParser(alphabet);
		SyntaxForest f = p.allParses(spec);
		Tree<String> morphtree=f.get(0);
		Tree<String> spectree=morphtree.getChildren().get(2).getChildren().get(0);
		Tree<String> ruletree=morphtree.getChildren().get(2).getChildren().get(1);
		Map<String,Set<String>> wordInfoSpec=wordInfoSpec(spectree);
		Set<Rule> rules=morphoRules(ruletree);
		return null;
	}
	private static Map<String,Set<String>> wordInfoSpec(Tree<String> spectree){
		
		HashMap<String,Set<String>> result= new HashMap<String,Set<String>>();
		if(spectree.value().equals("WordInfoEntry")) {
			Pair<String,Set<String>> entry = wordInfoEntry(spectree);
			result.put(entry.X,entry.Y);
		} else if(spectree.value().equals("WordInfoSpec")){
			for(Tree<String> child : spectree.getChildren())
				result.putAll(wordInfoSpec(child));
		}
		return result;
	}
	private static Pair<String,Set<String>> wordInfoEntry(Tree<String> infotree){
		// TODO : implement this
		return null;
	}
	private static Set<Rule> morphoRules(Tree<String> ruletree){
		HashSet<Rule> result= new HashSet<Rule>();
		if(ruletree.value().equals("Ruleset")) {
			Rule rule = ruleEntry(ruletree);
			result.add(rule);
		} else if(ruletree.value().equals("Rule")){
			for(Tree<String> child : ruletree.getChildren())
				result.addAll(morphoRules(child));
		}
		return result;
	}
	private static Rule ruleEntry(Tree<String> entrytree){
		// TODO :implement this
		return null;
	}
}
