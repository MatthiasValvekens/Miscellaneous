package grammar;
import java.util.Arrays;
import java.util.HashSet;
public class GrammarTest {
	public static void main(String[] args){
		HashSet<String> nonterm=new HashSet<String>(Arrays.asList("S","NP","VP","V","NP","P","NP","PP","Det","N"));
		HashSet<String> term=new HashSet<String>(Arrays.asList("eats","she","with","fish","fork","a"));
		HashSet<Rule> ruleset=new HashSet<Rule>();
		ruleset.add(Rule.fromString("S->NP VP"));
		ruleset.add(Rule.fromString("VP->VP PP"));
		ruleset.add(Rule.fromString("VP->V NP"));
		ruleset.add(Rule.fromString("VP->eats"));
		ruleset.add(Rule.fromString("PP->P NP"));
		ruleset.add(Rule.fromString("NP->Det N"));
		ruleset.add(Rule.fromString("NP->she"));
		ruleset.add(Rule.fromString("V->eats"));
		ruleset.add(Rule.fromString("P->with"));
		ruleset.add(Rule.fromString("N->fish"));
		ruleset.add(Rule.fromString("N->fork"));
		ruleset.add(Rule.fromString("Det->a"));
		
		ChomskyNormalGrammar cnf=new ChomskyNormalGrammar(term, nonterm, ruleset, "S");
		System.out.println(cnf.parse(new String[] {"she","eats","a","fish","with","a","fork"}));
		testCNFConvert();
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
}
