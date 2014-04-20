package compling.parsers;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import compling.grammar.ContextFreeGrammar;
import compling.grammar.ExtendedChomskyNormalGrammar;
import compling.grammar.Grammar;
import compling.grammar.Rule;
import compling.grammar.util.GrammarUtils;
import compling.grammar.util.SyntaxForest;
import compling.grammar.util.Tree;
import compling.morphology.Morphologist;
import compling.morphology.WordInfo;

public class Parser {
	private final Grammar syntax;
	private final Morphologist morphologist;
	private final Tokenizer tokenizer;
	private final Map<String,String> labelTranslator;
	private transient Grammar specializedParsingGrammar=null;
	public Parser(Grammar syntax, Morphologist morphologist){
		this(Tokenizer.getStandardTokenizer(),syntax,morphologist,GrammarUtils.UNIVERSAL_IDENTITY);
	}
	public Parser(Tokenizer t,Grammar syntax, Morphologist morphologist){
		this(t,syntax,morphologist,GrammarUtils.UNIVERSAL_IDENTITY);
	}
	public Parser(Tokenizer t, Grammar syntax, Morphologist morphologist,Map<String,String> labelTranslator){
		this.syntax=syntax;
		this.morphologist=morphologist;
		this.labelTranslator=labelTranslator;
		this.tokenizer=t;
	}
	public Tokenizer getTokenizer(){
		return tokenizer;
	}
	public Grammar getSyntax(){
		return syntax;
	}
	public Morphologist getMorphologist(){
		return morphologist;
	}
	//using the whole lexicon might be extremely slow in some cases, so we allow for specialisation
	//the default behaviour is simple: make the morphologist cough up the information about the individual words, and throw everything we find
	//into an ExtendedChomskyNormalGrammar
	protected Grammar getSpecializedParsingGrammar(Collection<String> words){
		if(specializedParsingGrammar==null){
			Set<Rule> rules=new HashSet<Rule>();
			Set<String> newterminals=new HashSet<String>(words);
			newterminals.addAll(syntax.getTerminalSymbols());
			
			for(String word : words){
				WordInfo info = morphologist.deinflect(word);
				//the syntax does all the additional checking for us, we just add all terminal identifiers the morphologist provides us with
				rules.add(new Rule(info.getSyntacticIdentifier(),word));
			}
			rules.addAll(syntax.getRules());
			ContextFreeGrammar base = ContextFreeGrammar.createContextFreeGrammar(newterminals, syntax.getNonterminalSymbols(), rules,syntax.getStartSymbols());
			specializedParsingGrammar=new ExtendedChomskyNormalGrammar(GrammarUtils.processUnitRules(base, false, false));
		}
		return specializedParsingGrammar;
	}
	public SyntaxForest allParses(List<String> words){
		return postProcess(getSpecializedParsingGrammar(words).allParses(words,labelTranslator));
	} 
	public SyntaxForest allParses(String data){
		return allParses(tokenizer.tokenize(data));
	}
	public Tree<String> bestParse(List<String> words){
		return allParses(words).get(0);
	}
	public Tree<String> bestParse(String data){
		return bestParse(tokenizer.tokenize(data));
	}
	protected SyntaxForest postProcess(SyntaxForest f){
		return f;
	}
}
