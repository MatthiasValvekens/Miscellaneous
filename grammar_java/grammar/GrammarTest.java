package grammar;
import grammar.util.GrammarUtils;

import java.util.List;
import java.io.*;
import grammar.util.Tree;

import java.util.Arrays;
import java.util.HashSet;

public class GrammarTest {
	public static void main(String[] args) throws Exception{
//		HashSet<String> nonterm=new HashSet<String>(Arrays.asList("S","NP","VP","V","NP","P","NP","PP","DET","N","CONJ","CP","ADJ","SUBC","COP","SP","INVS","PREDP"));
//		HashSet<String> term=new HashSet<String>(Arrays.asList("ik","lees","een","boek","voor","het","slapengaan","en","dat","interessant","is","leuk","Matthias","ben"));
//		HashSet<Rule> ruleset=new HashSet<Rule>();
//		ruleset.add(Rule.fromString("S->NP VP"));
//		ruleset.add(Rule.fromString("S->NP PREDP"));
//		ruleset.add(Rule.fromString("PREDP->COP NP"));
//		ruleset.add(Rule.fromString("PREDP->COP ADJ"));
//		ruleset.add(Rule.fromString("N->ADJ N"));
//		ruleset.add(Rule.fromString("INVS->ADJ COP"));
//		ruleset.add(Rule.fromString("VP->VP PP"));
//		ruleset.add(Rule.fromString("VP->V NP"));
//		ruleset.add(Rule.fromString("PP->P NP"));
//		ruleset.add(Rule.fromString("NP->DET N"));
//		ruleset.add(Rule.fromString("S->S CP"));
//		ruleset.add(Rule.fromString("CP->CONJ S"));
//		ruleset.add(Rule.fromString("N->N SP"));
//		ruleset.add(Rule.fromString("SP->SUBC INVS"));
//		ruleset.add(Rule.fromString("SUBC->dat"));
//		ruleset.add(Rule.fromString("CONJ->en"));
//		ruleset.add(Rule.fromString("ADJ->interessant"));
//		ruleset.add(Rule.fromString("NP->ik"));
//		ruleset.add(Rule.fromString("VP->lees"));
//		ruleset.add(Rule.fromString("V->lees"));
//		ruleset.add(Rule.fromString("P->voor"));
//		ruleset.add(Rule.fromString("DET->het"));
//		ruleset.add(Rule.fromString("N->slapengaan"));
//		ruleset.add(Rule.fromString("N->boek"));
//		ruleset.add(Rule.fromString("DET->een"));
//		ruleset.add(Rule.fromString("COP->is"));
//		ruleset.add(Rule.fromString("ADJ->leuk"));
//		ruleset.add(Rule.fromString("NP->Matthias"));
//		ruleset.add(Rule.fromString("COP->ben"));
//		
//		ChomskyNormalGrammar cnf=new ChomskyNormalGrammar(term, nonterm, ruleset, "S");
//		Tree<String> t= cnf.parse(new String[] {"ik","ben","Matthias","en","ik","lees","een","interessant","boek","dat","leuk","is","voor","het","slapengaan"});
//		System.out.println(t);
//		try{(new java.io.PrintStream(new java.io.FileOutputStream("syntax.gv"))).println(t.toDot("syntax_tree"));}catch(Throwable e){}
		testExtended();
		
		
	}
	
	public static void testCNFConvert(){
		HashSet<String> nonterm=new HashSet<String>(Arrays.asList("S","A","B"));
		HashSet<String> term=new HashSet<String>(Arrays.asList("a","b"));
		HashSet<Rule> ruleset=new HashSet<Rule>();
		ruleset.add(Rule.fromString("S->A S A"));
		ruleset.add(Rule.fromString("S->a B"));
		ruleset.add(Rule.fromString("A->B"));
		ruleset.add(Rule.fromString("A->S"));
		ruleset.add(Rule.fromString("B->b"));
		ruleset.add(Rule.fromString("B->"));
		System.out.println(ContextFreeGrammar.createContextFreeGrammar(term,nonterm,ruleset,"S").toCNF());
		
	}
	public static void testJapanese() throws Exception {
		java.util.Set<Rule> ruleset = GrammarUtils.rulesFromFile("japanesesyntax.txt");
		ChomskyNormalGrammar gram = ChomskyNormalGrammar.getSymbolsFromRules(ruleset, "S");
		System.out.println(gram);
		Tree<String> t =gram.parse(Arrays.asList("����","��","�w","��","�O","��","����","���[�","��","�V����","������","�y��","�ł�"));
		Writer w =new PrintWriter(new java.io.File("japanese.gv"),"UTF-8");
		w.write(t.toDot("syntax_tree","fontname=\"MS Mincho\""));
		w.close();
	}
	public static void testExtended() throws Exception{
		java.util.Set<Rule> ruleset = GrammarUtils.rulesFromFile("japanesesyntax2.txt");
		java.util.Set<Rule> extraunits = GrammarUtils.rulesFromFile("memberships.txt");
		ChomskyNormalGrammar gram = ChomskyNormalGrammar.getSymbolsFromRules(ruleset, "S");
		ruleset.addAll(extraunits);
		ExtendedChomskyNormalGrammar extgram=new ExtendedChomskyNormalGrammar(gram.getTerminalSymbols(), gram.getNonterminalSymbols(), ruleset, "S");
		//List<String> example=Arrays.asList("����","��","�w","��","�O","��","����","���[�","��","�V����","������","�y��","�ł�");
		List<String> example=Arrays.asList("�V�����@���́@�Ɓ@�Â��@���́@���@����".split("�@"));
		//List<String> example=Arrays.asList("����@�́@�V�����@�ł�".split("�@"));
		Tree<String> t =extgram.parse(example);
		Writer w =new PrintWriter(new java.io.File("extended.gv"),"UTF-8");
		w.write(t.toDot("syntax_tree","fontname=\"MS Mincho\""));
		w.close();
	}
}