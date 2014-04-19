package compling.grammar.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * A class that computes all possible partitions of a list into a given number of elements.
 * @author Matthias Valvekens
 * @version 1.0
 * @param <T>
 *     The type of elements of the list.
 */
public class Partitioner<T> implements Iterable<Partitioner.PartitionTriple<T>> {
	public static class PartitionTriple<T>{
		public final List<List<T>> part;
		public final List<Integer> starts;
		public final int groundsize;
		private PartitionTriple(List<List<T>> part,List<Integer> starts, int groundsize){
			this.part=part;
			this.starts=starts;
			this.groundsize=groundsize;
		}
		public String toString(){
			StringBuilder sb=new StringBuilder();
			int ssize=starts.size();
			for(int i=0;i<ssize-1;i++){
				sb.append(starts.get(i)+"--"+(starts.get(i+1)-1)+" ");
			}
			sb.append(starts.get(ssize-1)+"--"+(groundsize-1)+": ");
			sb.append(part.toString());
			return sb.toString();
		}
	}
	public final List<T> ground;
	public final List<PartitionTriple<T>> triples;
	public final int numpieces;
	/**
	 * Construct a new partitioner for the list <code>ground</code>.
	 * @param ground
	 * @param numpieces
	 */
	public Partitioner(List<T> ground, int numpieces){
		if(numpieces>ground.size()) throw new IllegalArgumentException("Partition count "+numpieces+" is greater than size "+ground.size());
		this.ground=ground;
		this.numpieces=numpieces;
		triples=Collections.unmodifiableList(build());
	}
	/**
	 * Returns an iterator over all partitions of <code>ground</code> that split <code>ground</code> into <code>numpieces</code> 
	 */
	@Override
	public Iterator<PartitionTriple<T>> iterator() {
		return triples.iterator();
	}
	private List<PartitionTriple<T>> build(){
		List<PartitionTriple<T>> res=new ArrayList<PartitionTriple<T>>();
		int gsize=ground.size();
		if(numpieces==1){
			List<List<T>> trivial=new ArrayList<List<T>>();
			trivial.add(ground);
			res.add(new PartitionTriple<T>(trivial,Arrays.asList(0),gsize));
			return res;
		}
		
		
		for(int start=numpieces-1;start<gsize;start++){
			List<T> curpiece=ground.subList(start,gsize);
			Iterator<PartitionTriple<T>> recursor=(new Partitioner<T>(ground.subList(0,start), numpieces-1)).iterator();
			while(recursor.hasNext()){
				PartitionTriple<T> subtriple=recursor.next();
				List<List<T>> thispartition=new ArrayList<List<T>>();
				thispartition.addAll(subtriple.part);
				thispartition.add(curpiece);
				List<Integer> starts= new ArrayList<Integer>(subtriple.starts);
				starts.add(start);
				PartitionTriple<T> thisTriple=new PartitionTriple<T>(thispartition,starts,gsize);
				res.add(thisTriple);
			}
		}
		return res;
	}
	
}
