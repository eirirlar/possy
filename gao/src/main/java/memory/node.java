package memory;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2007</p>
 * <p>Company: </p>
 * @author not attributable
 * @version 1.0
 */

import java.io.Serializable;
import java.util.*;

import fibHeapPackage.FibonacciHeapNode;
import utility.*;

public class node implements Serializable{

    //unique int id for one node
    public int id;
    
    public FibonacciHeapNode fibNode = null;
    
    private List<edge> outEdgesInGraph = new ArrayList<edge>();
    private List<edge> inEdgesInGraph = new ArrayList<edge>();
    private List<edge> edgesInSPT=new  ArrayList<edge>();
    edge preEdge; // for the trace back link in the fast path discovery
    edge preEdgeSideCost;//在side cost过程中使用的pre节点

    public int cost = Integer.MAX_VALUE; //cost for the DJ algorithm

    /**
     * spt 中源的level是1，每个点的level等于其父结点的level加1.
     */
    public int level = 0;

    public int pre, post, parent;

    public int sideCost=0;//for the topk query

    public int minSumSideCost=Integer.MAX_VALUE;

    public int hop = 0;

    
    public int treeLevel=0;
    public int inComingEdges=0;


    public node(int id1) {
        id = id1;
    }

    public void addOutEdgeIntoGraph(edge edge1) {
        if (!outEdgesInGraph.contains(edge1)) {
            outEdgesInGraph.add(edge1);
        }
    }

    public void addInEdgeIntoGraph(edge edge1) {
        if (!inEdgesInGraph.contains(edge1)) {
            inEdgesInGraph.add(edge1);
        }
    }


    public void addEdgeIntoSPT(edge edge1){
        if (!edgesInSPT.contains(edge1)) {
            edgesInSPT.add(edge1);
        }

    }

    public void addEdge(node toNode1, int cost) {
        edge newEdge = new edge();
        newEdge.fromNode=this;
        newEdge.toNode=toNode1;
        newEdge.distance = cost;
        this.addOutEdgeIntoGraph(newEdge);
        toNode1.addInEdgeIntoGraph(newEdge);

    }

    public void addChildNode(node childNode) {
        edge edge1 = new edge();
        edge1.fromNode = this;
        edge1.toNode = childNode;

        this.addOutEdgeIntoGraph(edge1);
        childNode.addInEdgeIntoGraph(edge1);
    }


    public void removeEdgeInSPT(edge edge1){
        boolean moved = edgesInSPT.remove(edge1);
        //System.out.println("removed edge is "+edge1.fromNode.id+"  "+edge1.toNode.id);
        if (!moved) {
            System.out.println("error in moving edge");
        }

    }

    public int getTotalEdge() {
        return outEdgesInGraph.size();
    }


    public Vector getToNodesString() {
        Vector toNodes = new Vector();
        edge cedge;
        for (int i = 0; i < outEdgesInGraph.size(); i++) {
            cedge =  outEdgesInGraph.get(i);
            toNodes.add(String.valueOf(cedge.getToNode()));
        }
        return toNodes;
    }

    public List<node> getToNodesList() {
        List<node> toNodes = new ArrayList<node>();
        edge cedge;
        for (int i = 0; i < outEdgesInGraph.size(); i++) {
            cedge = outEdgesInGraph.get(i);
            toNodes.add(cedge.toNode);
        }
        return toNodes;

    }

    public int[] getToNodes() {
        int[] toNodes = new int[outEdgesInGraph.size()];
        edge cedge;
        for (int i = 0; i < outEdgesInGraph.size(); i++) {
            cedge =  outEdgesInGraph.get(i);
            toNodes[i] = cedge.getToNodeID();
        }
        return toNodes;
    }

    public List<edge> getOutEdgesInGraph() {
        return outEdgesInGraph;
    }

    public List<edge> getInEdgesInGraph(){
        return inEdgesInGraph;
    }
    public List<edge> getEdgesInSPT(){
        return edgesInSPT;
    }

    public int getParent() {
        return parent;
    }

    public int getPreNodeID(){
        edge cedge=this.preEdge;
        if (cedge==null)
            return -1;
        if (cedge.fromNode==this)
            return cedge.toNode.id;
        else
            return cedge.fromNode.id;
    }

    /**
     * 返回到给点节点的边
     * @param toNodeID int
     * @return edge
     */
    public edge getEdgeFromToNodeInSPT(int toNodeID) {
        edge cedge;
        for (int i = 0; i < edgesInSPT.size(); i++) {
            cedge =  edgesInSPT.get(i);
            if (cedge.getToNodeID() == toNodeID) {
                return cedge;
            }
        }
        return null;
    }

//used:
    public int annnotateNode(int pre, int parent){
        this.parent=parent;
        int currentPre=pre;
        this.pre=currentPre;
        //this.
        node tNode;
        for (int i=0;i<this.edgesInSPT.size();i++){
            tNode=this.edgesInSPT.get(i).toNode;
            currentPre++;
            currentPre=tNode.annnotateNode(currentPre, this.id);
        }
        currentPre++;
        this.post=currentPre;
        return currentPre;
    }
    /**
     * clear edgesInSPT， and sideCost
     */
//used:
    public void clearForNextSPT(){
        edgesInSPT.clear();
        edge cedge;
        for (int i=0;i<outEdgesInGraph.size();i++){
            cedge=outEdgesInGraph.get(i);
            cedge.sideCost=0;
        }
        this.preEdge=null;
        this.cost=Integer.MAX_VALUE;
        this.hop=0;
    }

    public void setSideCost(){
        edge cedge;
        for (int i=0;i<outEdgesInGraph.size();i++){
            cedge=outEdgesInGraph.get(i);
            cedge.sideCost=cedge.distance-this.cost + cedge.toNode.cost;
        }
    }
}
