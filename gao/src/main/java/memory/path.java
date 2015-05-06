package memory;

/**
 * <p>Title: </p>
 *
 * <p>Description: </p>
 *  开始使用node来做内部的结构，不对，应该采用edge, 否则，无法恢复整个路径
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
     * path是根据parent 构建出来，前面的点cost比较大
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

    //在获取下一个路径的时候，不仅仅要考虑当前路径上的节点，而且要考虑所有topk的节点
    //就是保证在延伸下去的时候，应该不和以前的重复
    public List<path> getNextPaths(List<path> topks, int toID){
        List<path> nexts=new ArrayList<path>();
        List<node> nodes = new ArrayList<node>();//all nodes in current path, some of
        										 //them are to be explored to find a new path.
        edge cedge;
        //最后一个点实际上不需要考虑
        for (int i=0;i<edges.size();i++){
            cedge=edges.get(i);
            nodes.add(cedge.fromNode);
        }
        if (edges.size()>0){// no need to consider this node?
            cedge=edges.get(edges.size() -1);
            nodes.add(cedge.toNode);
        }

        //从startIdx开始枚举每个节点，获取这个节点到目标节点的最短路
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
            if (next!=null)  //有可能为空，
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
     * 每次路径未必都从source开始算，我们获取正确的，减少重复计算的启动点
     * 什么是正确的启动点
     * top 1的source node设置为开始的节点
     * 那么，在所有确定的topk路径中，source node前面的节点实际上都以及处理过了
     * 我们应该扫描现有的topk的路径，和当前路径，发现当前路径中最靠后的，和现有路径重复的，现有topk路径
     * source node之前的节点，作为我们正确的启动节点
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
                //一旦发现不相同，或者已经到达cpath的source node，则跳出内部循环
                //当前的cpath，只能支持到这个地方了
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
    //根据当前edges的情况，比对现有的topks数据，获取在扫描后续数据过程中不能出现的直接相连的数据
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
