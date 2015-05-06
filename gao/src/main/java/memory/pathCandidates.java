package memory;

/**
 * <p>Title: </p>
 *
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2009</p>
 *
 * <p>Company: </p>
 *
 * @author not attributable
 * @version 1.0
 */
import java.util.*;

import utility.*;

public class pathCandidates {
    List<List<path>> candidates = new ArrayList<List<path>>();
    int shortestDistance;
    int currentIdx;  //相当于二维表的行
    int currentPos=0; //相当于二维表的列
    int exactResults=0;
    static double app=1;
    int idx=0;
    
    /**
     * qiuhuida
     * @param shortest
     */
    
    public boolean enough(int k,int cur_num,int cur_len, List<path> topks)
    {
    	int i = 0;
    	int same_len1 = 0;
    	int same_len2 = 0;
    	path p;
    	for(i = 0; i < topks.size(); i++)
    	{
    		p = topks.get(i);
    		if(p.getLength() == cur_len)
    		{
    			same_len1++;
    		}
    	}
    	for(i = 0; i < candidates.size(); i++)
    	{
    		List<path> ps = candidates.get(i);
    		int j;
    		for(j = 0; j < ps.size(); j++)
    		{
    			p = ps.get(j);
    			if(p.getLength() == cur_len) same_len2 ++;
    		}
    	}
    	if(cur_num + same_len2 - same_len1 >k) return true;
    	
    	return false;
    }
    
    public pathCandidates(path shortest) {
         shortestDistance = shortest.totalDistance;
         this.addOneCandidatePath(shortest);
    }
    public path getCurrent(){
        if (currentIdx>candidates.size()){
            return null;  //已经没有了
        }
        if (idx ==6)
            idx=6;
        //Path是按照路径长度组织的
        List<path> currentResults= candidates.get(currentIdx);
        //path onePath= currentResults.remove(0);
        if (currentPos>=currentResults.size() ||  currentResults.size()==0){
            currentIdx++;
            while (currentIdx<candidates.size()){
                currentResults =candidates.get(currentIdx);
                if (currentResults.size()!=0){
                    break;
                }
                currentIdx++;
            }
            currentPos=0;
        }
        if (currentIdx ==candidates.size())
            return null;
        currentResults= candidates.get(currentIdx);
        path onePath = currentResults.get(currentPos);
        currentPos++;
        exactResults++;
        if (parameter.detail && idx >= 1){
            System.out.println(idx + " : " + onePath.toString());
        }
        idx++;
        return onePath; //这个是确定的top i的最短路径
    }

    public void addOneCandidatePath(path onePath){
        int idx=onePath.totalDistance-shortestDistance;
        List<path> results;
        if (idx>=candidates.size()){
            int itr = candidates.size()-1;
            for (int i= itr; i<idx;i++){
                results=new ArrayList<path>();
                candidates.add(results);
            }
            //currentPos=0;
        }
        results=candidates.get(idx);
        path cpath;
        for (int i=0;i<results.size();i++){
            cpath = results.get(i);
            if (cpath.toString().equalsIgnoreCase(onePath.toString())){
                return;
            }
        }
        //currentPos++;
        results.add(onePath);
        candidates.set(idx, results);
    }
    //无需判定重复，直接加入，yen的算法自身保证这一点
    public void addOneCandidatePathWithoutTesting(path onePath){
    		int i,j;
    		if(onePath.getLength() == 163)
    		{
    			i = 0;
    		}
    		for(i = 0; i < candidates.size(); i++)
    		{
    			List<path> ps = candidates.get(i);
    			for(j = 0; j < ps.size(); j++)
    			{
    				path p = ps.get(j);
    				if(p.isEqual(onePath)) return;
    			}
    			//if(ps.contains(onePath)) return;
    		}
            int idx=onePath.totalDistance-shortestDistance;
           
            List<path> results;
            if (idx>=candidates.size()){
                int itr = candidates.size()-1;
                for (i= itr; i<idx;i++){
                    results=new ArrayList<path>();
                    candidates.add(results);
                }
                //currentPos=0;
            }
            results=candidates.get(idx);
            results.add(onePath);
            candidates.set(idx, results);
    }
    public boolean enoughResults(int k){
        int maxLen=((int) ((currentIdx+shortestDistance)*app)) - shortestDistance;
        int sunResults = candidates.get(currentIdx).size() - currentPos;
        for (int i=currentIdx+1;i<maxLen && i<candidates.size();i++){
            sunResults=sunResults+candidates.get(i).size();
        }
        return ((exactResults+sunResults)>k);
    }

    public void outPutResult(){
        if (!parameter.detail){
            return;
        }
        int maxLen=((int) ((currentIdx+shortestDistance)*app)) - shortestDistance;
        int count=0;
        List<path> results;
        path onePath;
        for (int i=0;i<maxLen && i<candidates.size();i++){
            results = candidates.get(i);
            for (int j=0;j<results.size();j++){
                onePath = results.get(j);
                System.out.println(count+" : " +onePath.toString());
                count++;
            }
        }

    }
    public void outPutRestResult(int topk, int cur_num){
        if (!parameter.detail){
            return;
        }
        int maxLen=((int) ((currentIdx+shortestDistance)*app)) - shortestDistance;
        int count=exactResults;
        List<path> results;
        path onePath;
     
        for (int i=currentPos;currentIdx < candidates.size() && i<candidates.get(currentIdx).size() ;i++){
        	if(cur_num > topk) return;
            onePath = candidates.get(currentIdx).get(i);
            System.out.println(count+" : " +onePath.toString());
            count++;
            cur_num++;

        }
        currentIdx++;
        for (int i=currentIdx;i<maxLen && i<candidates.size();i++){
            results = candidates.get(i);
            for (int j=0;j<results.size();j++){
            	if(cur_num > topk) return;
                onePath = results.get(j);
                System.out.println(count+" : " +onePath.toString());
                count++;
                cur_num++;
            }
        }

    }

}
