//done!
package memory;

/**
 * <p>Title: </p>
 *
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2007</p>
 *
 * <p>Company: </p>
 *
 * @author not attributable
 * @version 1.0
 */
import java.util.*;

import fibHeapPackage.FibonacciHeap;
import fibHeapPackage.FibonacciHeapNode;

public class shortestPathTree {
    public static int OUT = 1;
    public static int IN = 0;
    node rootNode;
    int rootID;

    graph dgraph;

    List<node> activeNodesList = new ArrayList<node>();
    HashSet<node> nodesFinished = new HashSet<node>();
    FibonacciHeap<node> fibHeap = new FibonacciHeap<node>();
    Hashtable<Object, Object> fibnodesHash = new Hashtable<Object, Object>();

    /**
     * with length of the number of nodes. records the distance from every node to 
     * the target node.
     */
    int[] costs;
    
    List<node> leafNodesList = new ArrayList<node>();
    node mergeNode = null;
    int benefit = 0;
    int maxLevel = 0;
    int maxCost = 0;
    int totalNodes = 0;
    int direction = 1;


    public shortestPathTree(graph dgraph1, int rootID1, int direction1) {
        dgraph1.clearForNextSPT();
        rootID = rootID1;
        dgraph = dgraph1;
        dgraph.clearForNextSPT();
        rootNode = dgraph.getNodeById(rootID1);
        rootNode.cost = 0;
        direction = direction1;

    }

    /**
     * ���ڴ��й���spt�����ÿ������level����дcosts���顣
     * @param graph dataGraph
     */

    public void constructRevSPTInMem_Fib(){// --qiuhuida
    	node cnode;
        cnode = rootNode;
        nodesFinished.add(cnode);

        while (cnode != null) {
           
            extendInNodesInMemory_Fib(cnode);

            FibonacciHeapNode<node> n;
            n = fibHeap.removeMin();
            
            if(n == null) break;
            else
            {
	            cnode = n.getData();
	            cnode.fibNode = null;
            }
            nodesFinished.add(cnode);
        }
        initalCost(dgraph.nodeNum());

         this.visitTree();//����һ�������ҵ�����Ҷ�ӣ��Լ��ܵ����·,����costs����ĸ�ֵ
    }

	public void extendInNodesInMemory_Fib(node cnode)//--qiuhuida
    {
    	 int fromID, nextCost = 1;
         node fromNode;
         edge nextEdge;
         try {
             List<edge> rs;
             //�������б�
             rs = dgraph.getInEdge(cnode.id);
             edge edge;
             for (int i = 0; i < rs.size(); i++) {
                 edge = rs.get(i);
                 fromID = edge.fromNode.id;
                 nextCost = edge.distance;//distance from fromID to targetNode.

                 fromNode = dgraph.getNodeById(fromID);
                 if (fromNode != null && nodesFinished.contains(fromNode)) {
                     continue;
                 }

                 //the first time to be handled
                 if (fromNode.fibNode == null) {
                     fromNode = dgraph.getNodeById(fromID);//?? qiuhuida
                     //fromNode.preNode = cnode.id; //����ǰ��
                     fromNode.cost = cnode.cost + nextCost; //��������cost

                     nextEdge = new edge();   //�����µı�
                     //nextEdge.fromNodeID = cnode.id;  //����ߵ���Ϣ
                     nextEdge.fromNode  = cnode;
                     //nextEdge.toNodeID = fromNode.id;
                     nextEdge.toNode = fromNode;
                     nextEdge.distance=nextCost;
                     fromNode.preEdge = nextEdge; //����ǰ��
                     cnode.addEdgeIntoSPT(nextEdge);   //cnode����������ӵ�����
                  //   activeNodesList.add(fromNode); 
                     FibonacciHeapNode<node> n = 
                    	 new FibonacciHeapNode<node>(fromNode, fromNode.cost);
                     fromNode.fibNode = n;
                     fibHeap.insert(n, n.getKey());//��������
                     //nodesHash.put(toNode.id + "", toNode);    //����nodesHash�� �Ҹо�Ӧ�ð�nodeHashȥ��

                 } 
                 else if (fromNode.cost > cnode.cost + nextCost) { //may change the parenet node
                     //int pnodeID = fromNode.preNode;               //��ȡ��ǰ�ڵ��ԭ�е�parent�ڵ�
                     int pnodeID = fromNode.getPreNodeID();               //��ȡ��ǰ�ڵ��ԭ�е�parent�ڵ�
                     //node pnode = (node) nodesHash.get(pnodeID + "");
                     node pnode = dgraph.getNodeById(pnodeID);
                     nextEdge = pnode.getEdgeFromToNodeInSPT(fromID);

                     pnode.removeEdgeInSPT(nextEdge);           //�Ӵ����parent�ڵ���ɾ����

                     nextEdge.toNode = fromNode;
                     nextEdge.fromNode = cnode;
                     nextEdge.distance=nextCost;
                     //fromNode.preNode = cnode.id;
                     fromNode.preEdge = nextEdge;
                     cnode.addEdgeIntoSPT(nextEdge);
                     fromNode.cost = cnode.cost + nextCost;
                     fibHeap.decreaseKey(fromNode.fibNode, fromNode.cost);
                 }
                 
             }

         } catch (Exception e) {
             e.printStackTrace(System.out);
         }
    }

    void initalCost(int size) {//size <-- number of nodes.
        costs = new int[size];
        for (int i = 0; i < size; i++) {
            costs[i] = Integer.MAX_VALUE;
        }
    }

    public node getNodeWithMinCost() {
        if (activeNodesList.size() <= 0) {
            return null;
        }
        node nextNode = activeNodesList.get(0);
        node cnode;
        //һ������ҵ���С�����
        for (int i = 1; i < activeNodesList.size(); i++) {
            cnode = activeNodesList.get(i);
            if (cnode.cost < nextNode.cost) {
                nextNode = cnode;
            }
        }

        return nextNode;
    }
    void extendInNodesInMemory(node cnode) {
            int fromID, nextCost = 1;
            node fromNode;
            edge nextEdge;
            try {
                List<edge> rs;
                //�������б�
                rs = dgraph.getInEdge(cnode.id);
                edge edge;
                for (int i = 0; i < rs.size(); i++) {
                    edge = rs.get(i);
                    fromID = edge.fromNode.id;
                    nextCost = edge.distance;//distance from fromID to targetNode.

                    fromNode = dgraph.getNodeById(fromID);
                    if (fromNode != null && nodesFinished.contains(fromNode)) {
                        continue;
                    }

                    //the first time to be handled
                    if (!activeNodesList.contains(fromNode)) {
                        fromNode = dgraph.getNodeById(fromID);
                        fromNode.cost = cnode.cost + nextCost; //��������cost
                        nextEdge = new edge();   //�����µı�
                        nextEdge.fromNode  = cnode;
                        nextEdge.toNode = fromNode;
                        nextEdge.distance=nextCost;
                        fromNode.preEdge = nextEdge; //����ǰ��
                        cnode.addEdgeIntoSPT(nextEdge);   //cnode����������ӵ�����
                        activeNodesList.add(fromNode);             //��������
                    } else if (fromNode.cost > cnode.cost + nextCost) { 
                        int pnodeID = fromNode.getPreNodeID();               //��ȡ��ǰ�ڵ��ԭ�е�parent�ڵ�
                        node pnode = dgraph.getNodeById(pnodeID);
                        nextEdge = pnode.getEdgeFromToNodeInSPT(fromID);
                        pnode.removeEdgeInSPT(nextEdge);           //�Ӵ����parent�ڵ���ɾ����
                        nextEdge.toNode = fromNode;
                        nextEdge.fromNode = cnode;
                        nextEdge.distance=nextCost;
                        fromNode.preEdge = nextEdge;
                        cnode.addEdgeIntoSPT(nextEdge);
                        fromNode.cost = cnode.cost + nextCost;
                    }
                    
                }

            } catch (Exception e) {
                e.printStackTrace(System.out);
            }
        }

    /**
     * ����һ�������ҵ�����Ҷ�ӣ��Լ��ܵ����·
     * ����cost����ĸ�ֵ
     * @return int
     */
    public int visitTree() {

        LinkedList<node> wklist = new LinkedList<node>();
        wklist.add(rootNode);
        rootNode.cost=0;
        rootNode.parent=-1;
        rootNode.level = 1;
        int cnt = 0;
        int tempmaxLevel = 0;
        node cnode;
        int edgeCnt = 0;
        edge cedge;
        leafNodesList.clear();
        totalNodes = 0;
        while (!wklist.isEmpty()) {
            cnode = wklist.removeFirst();
            if (maxCost < cnode.cost) {
                maxCost = cnode.cost;
            }
            totalNodes++;
            //���·������
            this.costs[cnode.id] = cnode.cost;

            node childNode;
            edgeCnt = edgeCnt + cnode.getEdgesInSPT().size();
            if (cnode.getEdgesInSPT().size() == 0) {
                leafNodesList.add(0, cnode);
            }
            for (int i = 0; i < cnode.getEdgesInSPT().size(); i++) {
                cedge =  cnode.getEdgesInSPT().get(i);
                childNode = cedge.getToNode();
                childNode.cost=cnode.cost+cedge.distance;
                childNode.parent = cnode.id;
                wklist.add(childNode);
                childNode.level = cnode.level + 1;
                if (childNode.level > tempmaxLevel) {
                    tempmaxLevel = childNode.level;
                }
                cnt++;
            }
        }
        this.maxLevel = tempmaxLevel;
        
        return cnt;

    }

    public int getMaxCost() {
        return maxCost;
    }

    public int getMaxHeight() {
        return maxLevel;
    }

    public int getTotalNodes() {
        return totalNodes;
    }
    
    public node getRoot() {
        return rootNode;
    }


    /**
     * make the pre, post, parent annotation on each node(post is not calculated?)
     * ��һ��������pre, �ڶ���������-1;
     *
     */
    public void makePrePostParentAnnotation(){
        rootNode.annnotateNode(0, -1);
        return;
    }

}
