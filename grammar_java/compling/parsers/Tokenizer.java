package compling.parsers;

import java.util.List;
import java.util.ArrayList;
import java.util.regex.Pattern;

public abstract class Tokenizer {
	public static Pattern alphamatcher= Pattern.compile("^[A-Za-z0-9]+$");
	public static Tokenizer getStandardTokenizer(){
		// TODO implement this
		return null;
	}
	public static Tokenizer getGrammarTokenizer(){
		//for grammar and syntax specifications.
		//every alphanumeric string is one token
		//-> is one token
		//any other character is one token
		//spaces are always interpreted as token boundaries
		return new Tokenizer(){

			@Override
			public List<String> tokenize(String data) {
				ArrayList<String> res= new ArrayList<String>();
				String[] lines=data.split(System.lineSeparator());
				for(String line : lines){
					if(!line.isEmpty()){
						String[] blocks=line.split(" ");
						for(String block : blocks){
							
							StringBuilder sb = new StringBuilder();
							//-1:nothing/standby for new token
							//0: string
							//1: rule separator
							int status=-1;
							for(char c: block.toCharArray()){
								//is alphanumeric?
								boolean alpha = (c>=0x30 && c<=0x39) || (c>=0x41 && c<=0x5A) || (c>=0x61 && c<=0x7A);
								if(alpha){
									if(status==1){
										//this means the previous character was -, but the whole is not a rule separator
										res.add(sb.toString());
										sb=new StringBuilder();
									}
									status=0;
									sb.append(Character.toString(c));
								}
								else if(c=='-'){
									if(status!=-1){
										//end of previous token
										res.add(sb.toString());
										sb=new StringBuilder();
									}
									sb.append("-"); //just in case the next character turns out not to be >
									status=1;
								}
								else if(c=='>' && status==1){
									res.add("->");
									sb=new StringBuilder();
									status=-1;
								}
								else {
									//default : one "loose" character
									if(status!=-1){
										//end of previous token
										res.add(sb.toString());
										sb=new StringBuilder();
									}
									res.add(Character.toString(c));
								}
							}
							if(sb.length()>0) res.add(sb.toString());
						}
					}
					
					res.add("\n");
				}
				return res;
			}
			
		};
	}
	public abstract List<String> tokenize(String data);
}
