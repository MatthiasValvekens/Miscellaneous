package compling.morphology;

import java.util.Map;

public class WordInfo {
	private final Morphologist author;
	private final String word;
	private final Map<String,String> infoFields;
	private final int hashCode;
	private final String syntacticIdentifier;
	public WordInfo(Morphologist author, String word, Map<String,String> infoFields){
		if(word==null || infoFields==null || author==null) throw new NullPointerException();
		assert author.hasRequiredInformation(infoFields); //when doing high-volume parsing actually checking this would be impractical
		this.author=author;
		this.word=word;
		this.infoFields=infoFields;
		this.syntacticIdentifier=author.getSyntacticIdentifier(infoFields);
		hashCode=31*(31*author.hashCode()+word.hashCode())+infoFields.hashCode();
	}
	public Morphologist getAuthor(){
		return author;
	}
	public String getWord(){
		return word;
	}
	public String getSyntacticIdentifier(){
		return syntacticIdentifier;
	}
	public String getProperty(String key){
		return infoFields.get(key);
	}
	public boolean equals(Object o){
		if(o==this) return true;
		if(!(o instanceof WordInfo)) return false;
		WordInfo other=(WordInfo) o;
		return author.equals(other.author) && word.equals(other.word) && infoFields.equals(other.infoFields);
	}
	public int hashCode(){
		return hashCode;
	}
	
}
