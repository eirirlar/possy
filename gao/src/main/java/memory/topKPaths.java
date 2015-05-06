package memory;

/**
 * <p>Title: </p>
 *
 * <p>Description: </p>
 *  ��Ҫ�ǰ���yen�ķ���������top k��·������������ı�����һ������
 *  �������ò����ԣ�������path��list����ʾ
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

public class topKPaths {
    graph dgraph;
    path shortestOne;
    List<path> topks = new ArrayList<path>();
    List<path> candidates= new ArrayList<path>();
    int toID;
    //���Eppstein���㷨�����·����
    public static int COMBINEYENEPS=1;

    public topKPaths(graph dgraph1, path shortestOne1) {
        dgraph = dgraph1;
        shortestOne = shortestOne1;
        if (shortestOne.size()>0){
            edge last=shortestOne.get(shortestOne.size() -1) ;
            toID=last.toNode.id;
        }
    }
    //����ǰk��·��,Ŀǰ֧�����ַ���
    //���ǵķ�����combineYENEPS
    //��Yen OP�ķ���
    public List<path> buildTopKPathsEarly(int topk, int methodType){
        //�������·
        path selected;
        selected=shortestOne;
        //int [] testCosts= new int[2000];

        if(parameter.detail == true)
        	System.out.println("0 : "+selected.toString());
        topks.add(selected);
        int itr=0;
        pathCandidates paths = new pathCandidates(selected);
        //while (itr<topk){
        while (!paths.enoughResults(topk)){
            List<path> tmp=null; 
            if (methodType==topKPaths.COMBINEYENEPS){
                tmp = selected.getNextPaths(topks, toID);
            }

            path oneCandidate;
            for (int i = 0; i < tmp.size(); i++) {
                oneCandidate = tmp.get(i);
               
                paths.addOneCandidatePathWithoutTesting(oneCandidate);//actually tested! --qiuhuida
                
                
            }

            selected = paths.getCurrent();//output this path in this function.
            if (selected ==null)
                break;
       
            topks.add(selected);
            
            itr++;
        }
   //     System.out.println("Current Index is "+itr);
        paths.outPutRestResult(topk, itr);

        return topks;
    }


    public List<path> buildTopKPathsNormal(int topk, int methodType){
        //�������·
        path selected;
        selected=shortestOne;
        //int [] testCosts= new int[2000];

        if(parameter.detail == true)
        	System.out.println("Path 0 : "+selected.toString());
        topks.add(selected);
        int itr=0;//topk times exploration:
        while (itr<topk){
        	//explore new paths from current path 'selected':
            List<path> tmp=null;
            if (methodType==topKPaths.COMBINEYENEPS){
                tmp = selected.getNextPaths(topks, toID);
            }

            path oneCandidate;
            //add all new paths found from 'selected' (stored in tmp) into candidates:
            for (int i = 0; i < tmp.size(); i++) {
                oneCandidate = tmp.get(i);

                if (!this.isConstainedIn(candidates, oneCandidate) && !this.isConstainedIn(topks, oneCandidate)){
                        candidates.add(oneCandidate);
                    //    shortestPathTreeSideCost.totalCandidates ++;
                }
            }
            //��candidate��ѡ����һ����С��·��
            if (candidates.size() == 0)
                return topks;
            selected = candidates.get(0);
            for (int i = 1; i < candidates.size(); i++) {
                if (candidates.get(i).totalDistance < selected.totalDistance) {
                    selected = candidates.get(i);
                }
            }
            candidates.remove(selected);
            if (parameter.detail){
                System.out.println("Path " + (itr + 1) + " : " + selected.toString());
            }
            
            topks.add(selected);
            itr++;
        }

        return topks;
    }

    private boolean isConstainedIn(List<path> paths, path newPath){
        path cpath;
        for (int i=0;i<paths.size();i++){
            cpath=paths.get(i);
            if (cpath.isEqual(newPath))
                return true;
        }
        return false;
    }


}
