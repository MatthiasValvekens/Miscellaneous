package compling.morphology;

import compling.grammar.Grammar;

import java.util.Map;
import java.util.Set;

public abstract class Morphologist {
	// TODO : allow the Morphologist to compile a "syntactic lexicon" of sorts, where the whole ruleset is dumped somewhere
	protected final FiniteStateTransducer trans;
	private final Map<String,Set<String>> wordInfoMapper;
	
	public Morphologist(FiniteStateTransducer trans, Map<String,Set<String>> wordInfoMapper){
		this.trans=trans;
		this.wordInfoMapper=wordInfoMapper;
	}
	public FiniteStateTransducer getFiniteStateTransducer(){
		return trans;
	}
	public abstract WordInfo deinflect(String word);
	protected abstract String getSyntacticIdentifier(Map<String,String> info);
	//A morphologist's WordInfo objects should always provide a value within the relevant domain for each of these keys 
	public Set<String> getPropertyKeys(){
		return wordInfoMapper.keySet();
	}
	public Set<String> getPropertyDomain(String key){
		return wordInfoMapper.get(key);
	}
	public boolean hasRequiredInformation(Map<String,String> fields){
		if(!fields.keySet().equals(getPropertyKeys())) return false;
		for(String key : getPropertyKeys()){
			if(!getPropertyDomain(key).contains(fields.get(key))) return false;
		}
		return true;
	}
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((trans == null) ? 0 : trans.hashCode());
		result = prime * result
				+ ((wordInfoMapper == null) ? 0 : wordInfoMapper.hashCode());
		return result;
	}
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Morphologist other = (Morphologist) obj;
		if (trans == null) {
			if (other.trans != null)
				return false;
		} else if (!trans.equals(other.trans))
			return false;
		if (wordInfoMapper == null) {
			if (other.wordInfoMapper != null)
				return false;
		} else if (!wordInfoMapper.equals(other.wordInfoMapper))
			return false;
		return true;
	}	
}
