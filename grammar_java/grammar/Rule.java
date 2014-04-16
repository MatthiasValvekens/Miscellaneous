package grammar;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
public final class Rule {
	public static final String DEFAULT_TRANSFORM_SYMBOL="->";
	private final List<String> input;
	private final List<String> output;
	public Rule(String[] in, String[] out){
		this(Arrays.asList(in),Arrays.asList(out));
	}
	public Rule(List<String> in, List<String> out){
		input=Collections.unmodifiableList(in);
		output=Collections.unmodifiableList(out);
	}
	public static Rule fromString(String rule,String transformSymbol,String delimiter){
		String[] srule=rule.split(transformSymbol);
		return new Rule(srule[0].trim().split(delimiter),srule[1].trim().split(delimiter));
	}
	public static Rule fromString(String rule){
		return fromString(rule,DEFAULT_TRANSFORM_SYMBOL,Grammar.DEFAULT_DELIM);
	}
	
	public List<String> getInput(){
		return input;
	}
	
	public List<String> getOutput(){
		return output;
	}
	
	public String toString(){
		return toString(Grammar.DEFAULT_DELIM, DEFAULT_TRANSFORM_SYMBOL);
	}
	public String toString(String delim, String transformSymbol){
		return join(input,delim)+transformSymbol+join(output,delim);
	}
	public boolean equals(Object o){
		return o instanceof Rule && ((Rule) o).getInput().equals(input) && ((Rule) o).getOutput().equals(output);
	}
	public int hashCode(){
		return getInput().hashCode()*31+getOutput().hashCode();
	}
	
	//from stackexchange
	private static String join(List<String> strings, String delim) {
	    StringBuilder sb = new StringBuilder();
	    String sep="";
	    for(String s: strings) {
	        sb.append(sep).append(s);
	        sep=delim;
	    }
	    return sb.toString();                           
	}
}
