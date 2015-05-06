package memory;

/**
 * <p>Title: </p>
 *
 * <p>Description: </p>
 *  ��ʼʹ��node�����ڲ��Ľṹ�����ԣ�Ӧ�ò���edge, �����޷��ָ�����·��
 * <p>Copyright: Copyright (c) 2009</p>
 *
 * <p>Company: </p>
 *
 * @author not attributable
 * @version 1.0
 */
import java.util.*;

import utility.parameter;

public class path {
    static int count=0;
    graph dgraph;
    List<edge> edges=new ArrayList<edge>();
    //List<node> nodes = new ArrayList<node>();
    int totalDistance;

    node sourceNode=null;
    node cnode=null;
    int pathID=0;
    //added by qiuhuida:
    int cnodeSideCost = 0;
    public int getLength()
    {
    	return totalDistance;
    }
    /**
     * same path: return true;
     * else return false;
     * @param p
     * @return
     */

    public path(graph dgraph1) {
        dgraph=dgraph1;
        count++;
        pathID=count;
    }
    /**
     * path�Ǹ���parent ����������ǰ��ĵ�cost�Ƚϴ�
     * @param cnode node
     */
    public void addEdgeIntoPath(edge cedge){
        edges.add(cedge);
        totalDistance=this.getTotalDistance();
    }
    public void setSourceNode(node sourceNode1){
        sourceNode=sourceNode1;
    }
    public void setCnode(node cnode1){
        cnode = cnode1;
    }
    public int getTotalDistance(){
        int tot=0;
        for (int i=0;i<edges.size();i++){
            tot=tot+edges.get(i).distance;
        }
        return tot;
    }
    public void addEdgeFirst(edge cedge){
        edges.add(0, cedge);
        totalDistance=this.getTotalDistance();
    }

    public boolean isEqual(path second){
        if (this.size()!=second.size() || second.getLength() != this.getLength())
            return false;
        edge cedge1, cedge2;
        for (int i=0;i<edges.size();i++){
            cedge1=this.get(i);
            cedge2=second.get(i);
            if (cedge1.fromNode.id!=cedge2.fromNode.id || cedge1.toNode.id!=cedge2.toNode.id || cedge1.distance!=cedge2.distance)
                return false;
        }
        return true;
    }
    public String toString(){
        String content="";
        edge cedge;
        for (int i=0;i<edges.size();i++){
            cedge=edges.get(i);
            content=content+cedge.fromNode.id;
            content=content+"->";
        }
        if (edges.size()>0){
            cedge=edges.get(edges.size()-1);
            content=content+ cedge.toNode.id;
        }
        this.getTotalDistance();
        content=content+" (cost="+totalDistance+")";
        return content;
    }

    //�ڻ�ȡ��һ��·����ʱ�򣬲�����Ҫ���ǵ�ǰ·���ϵĽڵ㣬����Ҫ��������topk�Ľڵ�
    //���Ǳ�֤��������ȥ��ʱ��Ӧ�ò�����ǰ���ظ�
    public List<path> getNextPaths(List<path> topks, int toID){
        List<path> nexts=new ArrayList<path>();
        List<node> nodes = new ArrayList<node>();//all nodes in current path, some of
        										 //them are to be explored to find a new path.
        edge cedge;
        //���һ����ʵ���ϲ���Ҫ����
        for (int i=0;i<edges.size();i++){
            cedge=edges.get(i);
            nodes.add(cedge.fromNode);
        }
        if (edges.size()>0){// no need to consider this node?
            cedge=edges.get(edges.size() -1);
            nodes.add(cedge.toNode);
        }

        //��startIdx��ʼö��ÿ���ڵ㣬��ȡ����ڵ㵽Ŀ��ڵ�����·
        path next;
        if (edges.size()<1) return nexts;
        //int toId=nodes.get(nodes.size()-1).id;
        List<edge> removedEdges;
        int startIdx=this.getStartIdx(topks);//get the biggest node coinciding one path
        									 //in topks.
        for (int i=startIdx;i<nodes.size()-1;i++){//last one do not need to be considered. 
            removedEdges=getRemovedEdges(topks, i);
            
            shortestPathTreeSideCost sptsc =
            	new shortestPathTreeSideCost(dgraph, this, removedEdges,
            									nodes.get(i).id, toID);
            
            
            
            next=sptsc.buildNextShortestPath();
            if (next!=null)  //�п���Ϊ�գ�
            {
                nexts.add(next);
                //added by qiuhuida :
                
                shortestPathTreeSideCost.rtotalCandidates++;
                if(shortestPathTreeSideCost.maxThreshold < next.cnodeSideCost)
                {
                	shortestPathTreeSideCost.maxThreshold = next.cnodeSideCost;
                }
                
            }
            sptsc.resetSideCost();//set all sideCost to Integer.MAX_VALUE.
        }
        return nexts;
    }


       
    /**
     * ÿ��·��δ�ض���source��ʼ�㣬���ǻ�ȡ��ȷ�ģ������ظ������������
     * ʲô����ȷ��������
     * top 1��source node����Ϊ��ʼ�Ľڵ�
     * ��ô��������ȷ����topk·���У�source nodeǰ��Ľڵ�ʵ���϶��Լ��������
     * ����Ӧ��ɨ�����е�topk��·�����͵�ǰ·�������ֵ�ǰ·�������ģ�������·���ظ��ģ�����topk·��
     * source node֮ǰ�Ľڵ㣬��Ϊ������ȷ�������ڵ�
     * @param topks List
     * @return int
     */
    private int getStartIdx(List<path> topks){
        path cpath;
        int idx=0;
        edge cedge1, cedge2;
        
        for (int i=0;i<topks.size()-1;i++){
            cpath=topks.get(i);
            for (int j=0;j<cpath.edges.size() && j<this.size();j++){
                cedge1=cpath.edges.get(j);
                cedge2=this.get(j);
                //һ�����ֲ���ͬ�������Ѿ�����cpath��source node���������ڲ�ѭ��
                //��ǰ��cpath��ֻ��֧�ֵ�����ط���
                //if (cedge1.fromNode.id!=cedge2.fromNode.id || cedge1.fromNode==cpath.sourceNode){
                if (cedge1.fromNode.id!=cedge2.fromNode.id ){
                    break;
                }else{
                    if (j>idx){
                        idx=j;
                    }
                }
            }
        }
        return idx;
    }
    //���ݵ�ǰedges��������ȶ����е�topks���ݣ���ȡ��ɨ��������ݹ����в��ܳ��ֵ�ֱ������������
    //find the edges emitted from deviation node(e.g. idx) on any path in topk:
    private List<edge> getRemovedEdges(List<path> topks, int idx){
        path cpath;
        List<edge> nextEdges = new ArrayList<edge>();
        int j;
        for (int i=0;i<topks.size();i++){
            cpath=topks.get(i);
            for ( j=0;j<cpath.size() && j<idx;j++){
                if (!cpath.get(j).equal(edges.get(j))){
                    break;
                }
            }
            if (j==idx && cpath.size()>idx){
                if (!nextEdges.contains(cpath.get(idx))){
                    nextEdges.add(cpath.get(idx));
                }
            }
        }
        return nextEdges;

    }


    public int size(){
        return edges.size();
    }

    public edge get(int idx){
        return edges.get(idx);
    }

    public boolean isValidate(){//test if current path is a path;
        edge cedge1, cedge2;
        for (int i=0;i<edges.size()-1;i++){
            cedge1=edges.get(i);
            cedge2=edges.get(i+1);
            if (cedge1.toNode!=cedge2.fromNode){
                return false;
            }
        }
        return true;
    }

}
