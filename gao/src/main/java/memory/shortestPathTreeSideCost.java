package memory;

/**
 * <p>Title: </p>
 *
 * <p>Description: </p>
 *  ������tree,����û��tree�Ľṹ��������presidecost������
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


import utility.*;

//from Eppstain: sidecost(e) = cost(e) + d(head(e),t) - d(tail(e),t).
public class shortestPathTreeSideCost {

    node sourceNode;
    int sourceID;

    int targetID;
    node targetNode;

    graph dgraph;


    List<node> activeNodesList = new ArrayList<node>();//��Ż�Ҫ����sidecost��չ�Ľ�㣻
    List<node> nodesFinished = new ArrayList<node>();//�����չ��Ľ�㣬��activeNodesList�Ƴ�
    												 //�����nodesFinished�С�

    //����·���е�ǰ�沿�֣���Щ�ڵ㲻���ں����Ĵ�������г���
    List<node> removedNodes= new ArrayList<node>();
    
    FibonacciHeap<node> fibHeap = new FibonacciHeap<node>();

    int[] costs;

    List<node> leafNodesList = new ArrayList<node>();
    //for the merge greedy method
    path selected;

    List<edge> removedEdges;

    public static int sideCostThreshold=Integer.MAX_VALUE;
    int initValue=0;


    static int searchedNodes = 0;//added by qiuhuida
    public static int maxThreshold=0;
    public static int totalCandidates=0;
    public static int rtotalCandidates = 0;
    public static int EL = 0;
    //����Ǳ�����tree���������е���ȴ���2

    /**
     * Initialize this class, put the nodes before the currently being explored one(sourceID)
     * into removedNodes, calculate the starting sidecost of this subpath, stored in 
     * sourceNode.sideCost.
     */
    public shortestPathTreeSideCost(graph dgraph1, path selected1, List<edge> removedEdges1,  int sourceID1, int targetID1 ) {
        dgraph = dgraph1;
        selected=selected1;
        removedEdges = removedEdges1;

        sourceID = sourceID1;//sourceID is the node to be explored, not always the
        					 //source node in the original problem.
        sourceNode = dgraph.getNodeById(sourceID);

        targetID= targetID1;
        targetNode= dgraph.getNodeById(targetID);

        //��ʼ��ǰ���removed�ڵ�,��ѡ�е�·��selected��Դ�㵽��ǰ����չ��
        //���sourceNode֮������н�㶼Ӧ�ü��뵽removedNodes�С�
        edge cedge;
        for (int i=0;i<selected.size();i++){
            cedge = selected.get(i);
            if (selected.get(i).fromNode==sourceNode){
                removedNodes.add(selected.get(i).fromNode);
                break;
            }else{
                initValue = initValue+ cedge.sideCost;
                removedNodes.add(selected.get(i).fromNode);
            }
        }
        //ע���Ǹ��ӵ�cost
        sourceNode.sideCost = initValue;
        selected=selected1;//redundant?
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
              if (cnode.sideCost < nextNode.sideCost ) {
                  nextNode = cnode;
              }
          }
          
          return nextNode;
      }

      private boolean isValidateCandidate(node cnode){
          return ! removedNodes.contains(cnode);
      }
      public void resetSideCost(){
          node cnode;
          for (int i=0;i<activeNodesList.size();i++){
              cnode=activeNodesList.get(i);
              cnode.sideCost=Integer.MAX_VALUE;
          }
          for (int i=0;i<nodesFinished.size();i++){
              cnode=nodesFinished.get(i);
              cnode.sideCost=Integer.MAX_VALUE;
          }

      }
      
      public void extendNodesInMemory_Fib(node cnode)
      {
    	  int toID, nextCost = 1;
          node toNode;
          edge nextEdge;
          try {
              List<edge> rs;
              //�������б�
              rs = dgraph.getOutEdge(cnode.id);

              edge edge;
              //iterate all edges emitted from cnode:
              for (int i = 0; i < rs.size(); i++) {
                  edge = rs.get(i);
                  //��������ߺ���һ��һ�����򲻿���
                  if (this.isRemovedNextEdge(edge))
                      continue;
                  toID = edge.toNode.id;
                  
                  nextCost = edge.sideCost;
                  toNode = dgraph.getNodeById(toID);
                  //�������������ǰ�������򲻿���
                  if (!this.isValidateCandidate(toNode))
                      continue;

                  if (toNode != null && nodesFinished.contains(toNode)) {
                      if (toNode.treeLevel>cnode.treeLevel){
                         toNode.inComingEdges++;
                     }

                      continue;
                  }
                  //the first time to be handled
                  if (toNode.fibNode == null) {
                      toNode = dgraph.getNodeById(toID);
                      toNode.preEdgeSideCost = edge; //����ǰ��
                      toNode.sideCost = cnode.sideCost + nextCost; //��������cost
                      if(toNode.sideCost < 0)
                      {
                      	toNode.sideCost = Integer.MAX_VALUE;
                      }
                      toNode.treeLevel = cnode.treeLevel+1;
                      toNode.inComingEdges=1; //��ʼ��������һ��
                      if (toNode.sideCost <= this.sideCostThreshold){//'=':added by qiuhuida.
                       //   activeNodesList.add(toNode); //��������
                    	  FibonacciHeapNode<node> n = new FibonacciHeapNode<node>(toNode,toNode.sideCost);
                    	  toNode.fibNode = n;
                    	  fibHeap.insert(n, n.getKey());
                      }

                  } else if (toNode.sideCost > cnode.sideCost + nextCost) { //may change the parent node
                      int pnodeID = toNode.preEdgeSideCost.fromNode.id;//��ȡ��ǰ�ڵ��ԭ�е�parent�ڵ�
                      toNode.preEdgeSideCost = edge;
                      toNode.treeLevel = cnode.treeLevel+1;
                      toNode.sideCost = cnode.sideCost + nextCost;
                      fibHeap.decreaseKey(toNode.fibNode, toNode.sideCost);
                  }
                  

              }

          } catch (Exception e) {
              e.printStackTrace(System.out);
          }
      }
    /**
     * ��fib_dj�㷨�����·����
     * @ change the name from constructShortPathTreeToNodeInMemory to find shortusedj --jiangxiao
     * @param graph dataGraph
     * @param toID int
     * @return int
     */
    public path buildNextShortestPath() {

        node cnode;
        cnode = sourceNode;
        if(cnode.id == 3)
        	cnode.id = 3;
        FibonacciHeapNode<node> n = new FibonacciHeapNode<node>(cnode, cnode.sideCost);
        cnode.fibNode = n;
        fibHeap.insert(n, n.getKey());
        path next = null;
        int firstTime=0;
        while (cnode != null) {
        	
            extendNodesInMemory_Fib(cnode);//��cnode����ı��п���չ�Ľ�����activeNodesList��
            							//��Ҫʱ�޸Ľ���sidecost��
            fibHeap.delete(cnode.fibNode);
            cnode.fibNode = null;
            nodesFinished.add(cnode);
            if(fibHeap.min()!= null)
            {
            	cnode = fibHeap.min().getData();
            }
            else cnode = null;
            
            if (cnode == null) {
            	//��fibHeap�еĽ���fibNode���null:
            	while(fibHeap.isEmpty() == false)
            	{
            		fibHeap.min().getData().fibNode = null;
            		fibHeap.removeMin();
            	}
            	return next;//added by qiuhuida.
            }
            
            if (this.isTerminate(cnode)) {
                if (firstTime ==0){
                    next = this.generatePath(cnode);    //nextֻ������̵�·��
                    if(next != null)
                    {
                    	next.cnodeSideCost = cnode.sideCost;//added by qiuhuida.
                    	firstTime++;
                    }
                }

                //����Ǽ�֦���ԣ�
                
                if(parameter.pruningNodes)
                {
                	if(EL == 1)
                	{
	                	if (rtotalCandidates == parameter.topks) {
	                		if(shortestPathTreeSideCost.sideCostThreshold > maxThreshold)
	                		{
	                			shortestPathTreeSideCost.sideCostThreshold = maxThreshold;
	                		//	System.out.println("The threshold : "+maxThreshold);
	                		}
	                		//set the following to zero, restart finding a new threshold:
	                		rtotalCandidates = 0;
	                		maxThreshold = 0;
	                		
	                	}
	                	break;
                	}
                	if(EL == 0)
                	{
	                    if (totalCandidates >= parameter.topks) {
	                    	//�������У���һ����ֵ��ȡ�ɹ�����ȡ��һ����ֵ��
	                    	//totalCandidates = 0;
	                    	//maxThreshold = 0;
	                        break; //ֱ���˳�
	                    }else {
	                        totalCandidates = totalCandidates +
	                                          this.getIncomingEdgeCombations(cnode);
	                        //totalCandidates=totalCandidates+1;
	                        //System.out.println("source node is "+sourceNode.id);
	                        if (cnode.sideCost > maxThreshold) { //������ֵ
	                            maxThreshold = cnode.sideCost;
	                        }
	                        if (totalCandidates >= parameter.topks) {
	                        	//added by qiuhuida:
	                        	if(shortestPathTreeSideCost.sideCostThreshold > maxThreshold)
	                        	{
	                        		shortestPathTreeSideCost.sideCostThreshold = maxThreshold;
	                        	}
	                        }
	                        
	                    }
                	}
                    
                } else { //���Ǽ�֦���ԣ�ֱ���˳�
                    break;
                }
            }
        }

        if (this.isTerminate(cnode) && next==null) {
        	next = this.generatePath(cnode);
        	if(next != null)//added by qiuhuida.
            {
            	next.cnodeSideCost = cnode.sideCost;
            }
        }

        //��fibHeap�еĽ���fibNode���null:
    	while(fibHeap.isEmpty() == false)
    	{
    		fibHeap.min().getData().fibNode = null;
    		fibHeap.removeMin();
    	}
        return next;
    }

    /**
     * �ж�������cnode��target node�ĺ�������ǣ�����source node�ĺ��
     * @param cnode node
     * @return boolean
     */
    private boolean isTerminate(node cnode){
        /*if (cnode.pre>=targetNode.pre && cnode.post<=targetNode.post){
            if (!(cnode.pre >= sourceNode.pre && cnode.post <= sourceNode.post))
                return true;
        }*/
        //return false;
    	///////////////////////////////////////////////////////////////////////////////////////
    	/*
    	 * added by qiuhuida: check if cnode is targetNode's offspring?
    	 */
    	if( !(cnode.pre>=targetNode.pre && cnode.post<=targetNode.post))
    		return false;
    	///////////////////////////////////////////////////////////////////////////////////////
        node rnode;
        for (int i=0;i<removedNodes.size();i++){
            rnode=removedNodes.get(i);
            if (cnode.pre >= rnode.pre && cnode.post <= rnode.post)
                return false;
        }
        return true;

    }
    //path��Ϊ���Σ�seleted��һֱ��source��һ�Σ�source��cnode��һ�Σ�cnode��target��һ��
    //
    public  path generatePath(node cnode){
        path.count++;
        if (cnode==null || sourceNode ==cnode)//?????????????????????????????????????????????
            return null;
        path spath=new path(dgraph);
        //��һ��
        for (int i=0;i<selected.size();i++){
            if (selected.get(i).fromNode==sourceNode){
                //spath.addNodeIntoPath(selected.get(i));
                break;
            }else
                spath.addEdgeIntoPath(selected.get(i));
        }
        spath.setSourceNode(sourceNode);
        //�ڶ���
        path second=new path(dgraph);
        edge tmp=cnode.preEdgeSideCost;
        while (tmp.fromNode!=sourceNode){
            second.addEdgeFirst(tmp);
            tmp=tmp.fromNode.preEdgeSideCost;
        }
        spath.addEdgeIntoPath(tmp);
        /*for (int i=second.size()-1;i>=0;i--){
            spath.addEdgeIntoPath(second.get(i));
        }*/
        for (int i=0;i<second.size();i++){
            spath.addEdgeIntoPath(second.get(i));
        }

        spath.setCnode(cnode);
        //������  ����һ�αȽ��ң���Ҫ������������Щ��
        tmp=cnode.preEdge;//preEdge������spt��target���ΪԴ��
        				  //�д�cnode��target���·����cnode��һ����㡣
        /*node tnode;
        if (tmp.fromNode==cnode){
            tnode=tmp.toNode;
        }else{
            tnode=tmp.fromNode;
        }*/
        if (tmp!=null){
            tmp = tmp.reverseEdge();
            while (tmp.toNode.id != targetID) {
                spath.addEdgeIntoPath(tmp);
                tmp = tmp.toNode.preEdge;
                if (tmp==null)
                    tmp=null;
                tmp = tmp.reverseEdge();
            }
            spath.addEdgeIntoPath(tmp);
        }else{
        	if(cnode.id != targetNode.id)//added by qiuhuida.
        	{
        		return null;//����һ���걸��path
        	}
        }
        //System.out.println(spath.toString());
        if (!spath.isValidate()){
            System.out.println("current wrong path is "+spath.toString());
        }
        return spath;

    }

    private boolean isRemovedNextEdge(edge cedge){
        /*int idx=-1;
        for (int i=0;i<selected.size();i++){
            if (selected.get(i).fromNode==sourceNode){
                idx=i;
                break;
            }
        }
        if (idx == -1)
            return false;
        edge edgeInPath=selected.get(idx);
        if (cedge.fromNode==sourceNode && cedge.toNode==edgeInPath.toNode && cedge.distance==edgeInPath.distance)
             return true;*/
        edge removed;
        for (int i=0;i<removedEdges.size();i++){
            removed=removedEdges.get(i);
            if (removed.equal(cedge))
                return true;
        }
        return false;
    }

    public void setSideCostTreshold(int maximalCost ){
        sideCostThreshold = maximalCost;
    }
    
    /*
     * ��cnode����ı��п���չ�ķ���activeNodesList�У���Ҫʱ�޸Ľ���sidecost��
     */
    void extendNodesInMemory(node cnode) {
        int toID, nextCost = 1;
        node toNode;
        edge nextEdge;
        try {
            List<edge> rs;
            //�������б�
            rs = dgraph.getOutEdge(cnode.id);

            edge edge;
            //iterate all edges emitted from cnode:
            for (int i = 0; i < rs.size(); i++) {
                edge = rs.get(i);
                //��������ߺ���һ��һ�����򲻿���
                if (this.isRemovedNextEdge(edge))
                    continue;
                toID = edge.toNode.id;
                
                nextCost = edge.sideCost;
                toNode = dgraph.getNodeById(toID);
                //�������������ǰ�������򲻿���
                if (!this.isValidateCandidate(toNode))
                    continue;

                if (toNode != null && nodesFinished.contains(toNode)) {
                    if (toNode.treeLevel>cnode.treeLevel){
                       toNode.inComingEdges++;
                   }

                    continue;
                }
                //the first time to be handled
                if (!activeNodesList.contains(toNode)) {
                    toNode = dgraph.getNodeById(toID);//redundant?
                    toNode.preEdgeSideCost = edge; //����ǰ��
                    toNode.sideCost = cnode.sideCost + nextCost; //��������cost
                    if(toNode.sideCost < 0)//added by qiuhuida;
                    {
                    	toNode.sideCost = Integer.MAX_VALUE;
                    }
                    toNode.treeLevel = cnode.treeLevel+1;
                    toNode.inComingEdges=1; //��ʼ��������һ��
                    if (toNode.sideCost <= this.sideCostThreshold){//'=':added by qiuhuida.
                        activeNodesList.add(toNode); //��������
                    }
                    //nodesHash.put(toNode.id + "", toNode);    //����nodesHash�� �Ҹо�Ӧ�ð�nodeHashȥ��

                } else if (toNode.sideCost > cnode.sideCost + nextCost) { //may change the parent node
                    int pnodeID = toNode.preEdgeSideCost.fromNode.id;               //��ȡ��ǰ�ڵ��ԭ�е�parent�ڵ�
                    //node pnode = (node) nodesHash.get(pnodeID + "");
                    //node pnode = dgraph.getNodeById(pnodeID);
                    toNode.preEdgeSideCost = edge;
                    toNode.treeLevel = cnode.treeLevel+1;
                    toNode.sideCost = cnode.sideCost + nextCost;
                }
                //ʵ���ϣ���һ��hash�ǹ��õģ�����active nodes list��finished hash, ���԰�nodeHashȥ����

            }

        } catch (Exception e) {
            e.printStackTrace(System.out);
        }
    }

    private int getIncomingEdgeCombations(node cnode){
        edge tmp=cnode.preEdgeSideCost;
        node before = tmp.fromNode;
        if (before.pre>cnode.pre && before.post<cnode.post){ //before��cnode���ӽڵ�
            return 0;   //��������������µ�·����before�ڵ�����ĺ�cnode��������һ������
        }
        while (before!=sourceNode){
            if (before.pre<cnode.pre && before.post>cnode.post){ //before��cnode���ӽڵ�
                return 0;   //��������������µ�·����before�ڵ�����ĺ�cnode��������һ������
            }
            tmp = before.preEdgeSideCost;
            before = tmp.fromNode;
        }
        return 1;
        /*int tot=1;
        while (tmp.fromNode!=sourceNode){
            //second.addEdgeFirst(tmp);
            tot = tmp.fromNode.inComingEdges* tot;
            tmp=tmp.fromNode.preEdgeSideCost;
        }
        if (tot>1){
            System.out.println("total combinations are "+tot);
        }

        return tot;
        */
    }
    public static void resetStaticForNextTime(){
        sideCostThreshold=Integer.MAX_VALUE;
        maxThreshold=0;
        totalCandidates=0;
    }

}
