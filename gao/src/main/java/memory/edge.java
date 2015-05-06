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

public class edge implements Serializable {


    public node fromNode;

    public node toNode = null;

    public int distance;

    public int sideCost=0;

    public edge() {

    }

     public void setFromNode(node fromNode1) {
        fromNode = fromNode1;
    }

    public void setToNode1(node toNode1) {
        toNode = toNode1;
    }

    public int getToNodeID() {
        return toNode.id;
    }

    public node getToNode() {
        return toNode;
    }

    public boolean equal(edge edge1) {
        if (edge1.fromNode.id == this.fromNode.id &&
            edge1.toNode.id == this.toNode.id) {
            return true;
        } else {
            return false;
        }
    }

    public edge reverseEdge(){
        edge redge=new edge();
        redge.fromNode=this.toNode;
        redge.toNode=this.fromNode;
        redge.distance=this.distance;
        return redge;
    }

    public String toString(){
        String content="";
        content=" from="+this.fromNode.id + " to="+this.toNode.id +" distance="+ this.distance+ " sidetrack="+ this.sideCost;
        return content;
    }


}
