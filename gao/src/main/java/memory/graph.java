package memory;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2007</p>
 * <p>Company: </p>
 * @author not attributable
 * @version 1.0
 */
import java.io.*;
import java.util.Random;

import database.*;
import utility.*;
import java.sql.Statement;
import java.sql.ResultSet;
import java.util.*;


public class graph {

    public List<node> nodes = new ArrayList<node>();
    int size = 0;

    public int nodeNum() {
        return size;
    }

    public graph(int size1) {
        for (int i = 0; i < size1; i++) {
            nodes.add(new node(i));
        }
        size = size1;
    }
//used:
    public void constructEdgsFromDB(Statement sta, String prefix) {
        try {
            String SQLState = " select fromnode, tonode, cost from " + prefix +
                              "edge";
            ResultSet rs;
            try {
                rs = sta.executeQuery(SQLState);
            } catch (Exception e1) {
                database.restartConnection();
                rs = database.sta.executeQuery(SQLState);
            }
            String fromID, toID;
            int cost;
            node cnode;
            while (rs.next()) {
                fromID = rs.getString("fromNode").trim();
                toID = rs.getString("toNode").trim();
                cost = rs.getInt("cost");
                Integer ID = new Integer(fromID.trim());
                cnode = nodes.get(ID.intValue());
                ID = new Integer(toID.trim());
                cnode.addEdge(nodes.get(ID.intValue()), cost);
            }
        } catch (Exception e) {
            e.printStackTrace(System.out);
        }
    }

    /**
     * 返回一个节点相邻的边
     * @param fromID String
     * @return List
     */
    public List<edge> getOutEdge(int fromID) {
        node cnode = nodes.get(fromID);
        return cnode.getOutEdgesInGraph();
    }

    public List<edge> getInEdge(int toID) {
        node cnode = nodes.get(toID);
        return cnode.getInEdgesInGraph();
    }

//used
    public List<node> getNodes() {
        return nodes;
    }
//used
    public node getNodeById(int id) {
        if (nodes == null)
            return null;
        return nodes.get(id);
    }


//used
    public void clearForNextSPT(){
        node cnode;
        for (int i=0;i<nodes.size();i++){
            cnode=nodes.get(i);
            cnode.clearForNextSPT();
        }
    }

    /**
     * set sideCost to every node, according to Eppstein.
     */
//used:
    public void setSideCost(){
        node cnode;
        for (int i=0;i<nodes.size();i++){
            cnode=nodes.get(i);
            cnode.setSideCost();//from Eppstein.
        }
    }
//used:
    public path getShorestPath(int fromNode, int toNode){
        node fnode, tnode;
        fnode=nodes.get(fromNode);
        tnode=nodes.get(toNode);
        path spath= new path(this);
        node tmp=fnode;
        edge cedge;
        while (tmp.parent!=-1){
            cedge=new edge();
            cedge.fromNode=tmp;
            cedge.toNode= tmp.preEdge.toNode;
            cedge.distance = tmp.preEdge.distance;
            tmp=this.getNodeById(tmp.parent);
            cedge.toNode =tmp;
            spath.addEdgeIntoPath(cedge);
        }
        return spath;
    }

//used:
    public void buildTopKPaths(int fromNode, int toNode, int topk,  int methodType){
       path spath=this.getShorestPath(fromNode, toNode);
       spath.setSourceNode(this.getNodeById(fromNode));
       topKPaths paths= new topKPaths(this,spath );
       if (parameter.earlyTerminate){
            paths.buildTopKPathsEarly(topk, methodType);
       }else{
            paths.buildTopKPathsNormal(topk, methodType);
       }
       spath = null;
       paths = null;
       System.gc();
   }
    
    public void resetNodeCost(){
        node cnode;
        for (int i=0;i<nodes.size();i++){
            cnode=nodes.get(i);
            cnode.cost=Integer.MAX_VALUE;
            cnode.parent =-1;
            cnode.preEdge = null;
        }
    }


}
