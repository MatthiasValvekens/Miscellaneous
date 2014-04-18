package grammar;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
public final class Rule {
	public static final String DEFAULT_TRANSFORM_SYMBOL="->";
	private final List<String> input;
	private final List<String> output;
	private final int hashCode;
	public Rule(String[] in, String[] out){
		this(Arrays.asList(in),Arrays.asList(out));
	}
	public Rule(String in, List<String> out){
		this(Arrays.asList(in),out);
	}
	public Rule(List<String> in, String out){
		this(in,Arrays.asList(out));
	}
	public Rule(String in, String out){
		this(Arrays.asList(in),Arrays.asList(out));
	}
	public Rule(List<String> in, List<String> out){
		input=Collections.unmodifiableList(in);
		output=Collections.unmodifiableList(out);
		hashCode=getInput().hashCode()*31+getOutput().hashCode();
	}
	public static Rule fromString(String rule,String transformSymbol,String delimiter){
		if(!rule.contains(transformSymbol)) throw new IllegalArgumentException("Rule does not contain transform symbol");
		String[] srule=rule.split(transformSymbol);
		String[] in=srule[0].trim().split(delimiter);
		String[] out= srule.length>1 ? (srule[1].trim().split(delimiter)) : new String[0];
		
		return new Rule(in,out);
	}
	public Rule(Rule r){
		this(new java.util.ArrayList<String>(r.getInput()),new java.util.ArrayList<String>(r.getOutput()));
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
		return hashCode;
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
