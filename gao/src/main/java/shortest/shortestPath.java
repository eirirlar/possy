package shortest;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2007</p>
 * <p>Company: </p>
 * @author not attributable
 * @version 1.0
 */

import java.io.*;
import memory.*;
import utility.*;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;


public class shortestPath {

    int queryNumber = 100;

    long simpleTimes1 = 0, simpleTimes2 = 0, indexTimes = 0;
    int simpleDistance1 = 0, simpleDistance2 = 0, indexDistance = 0;

    int source=0, target=9999;

//used
    public void TopKShortestPath(graph graph) {
    	try{
    		this.ourMemoryTopK(graph, source, target);
    	}catch(IOException e) {
    		e.printStackTrace();
    	}
    }
//used:
    public void ourMemoryTopK(graph dgraph, int source, int target) throws IOException{
        long startTime1, startTime2;
        shortestPathTree spt;

        String content="";
        System.out.println("Begin our method-------------------");
        startTime1 = System.currentTimeMillis();
        //spt = new shortestPathTree(graph, results.nodes2[i], shortestPathTree.IN);
        spt = new shortestPathTree(dgraph, target, shortestPathTree.IN);//IN equals 0, construct reversed spt;
        //构建完整的逆向spt树
        System.out.println("Constructing SPT...");
        spt.constructRevSPTInMem_Fib(); //Fib DJ. function added by qiuhuida
        System.out.println("Making PrePostParentAnnotations...");
        spt.makePrePostParentAnnotation();
        //endTime = System.currentTimeMillis();
        content=content+" Build First Shortest Path Tree="+(System.currentTimeMillis() -startTime1);
        long spttime = (System.currentTimeMillis() -startTime1);

        System.out.println("Setting extra costs...");
        dgraph.setSideCost();//sideCost: Eppstein's sideCost.


        startTime2=System.currentTimeMillis();
        shortestPathTreeSideCost.resetStaticForNextTime();
        if (!parameter.pruningNodes){
            //path spath =dgraph.getShorestPath(source, target);
            shortestPathTreeSideCost.sideCostThreshold = Integer.MAX_VALUE;
        }

        //generate and output top k sp:
        System.out.println("Building shortest paths...");

        FileWriter output = new FileWriter("testLogs.txt",true);
        output.write("\r\n===========================================================\r\n");
        SimpleDateFormat sf = new SimpleDateFormat("yyyy-MM-dd");   
        Date date = new Date();                                                                 
        System.out.println(sf.format(date)); 
        output.write("Date : "+sf.format(date)+"\r\n");
        output.write("Dataset : "+parameter.prefix+"\r\n");

         int ii,jj;
         for(jj = 0; jj <=1; jj++)
         {
        	 if(jj == 0) parameter.earlyTerminate = false;
        	 else parameter.earlyTerminate = true;
	         for(ii = 0; ii <3; ii++)
	         {
	        	 System.out.println("===========================================================");
	             output.write("\r\n===========================================================\r\n");
	             if(ii == 0)
	             {
	                 parameter.pruningNodes = false;
	                 if(parameter.earlyTerminate == true)
	                 {
		                 System.out.println("Testing KR: k-reduction strategy, without pruning optimizations");
		                 output.write("Testing KR: k-reduction strategy, without pruning optimizations\r\n");
	                 }
	                 else
	                 {
	                	 System.out.println("Testing NM: normal termination, without pruning optimizations");
		                 output.write("Testing NM: normal termination, without pruning optimizations\r\n");
	                 }
	             }
	             if(ii == 1)
	             {
	                 parameter.pruningNodes = true;
	                 shortestPathTreeSideCost.EL = 0;
	                 if(parameter.earlyTerminate == true)
	                 {
		                 System.out.println("Testing KRE: k-reduction with Eager Strategy");
		                 output.write("Testing KRE: k-reduction with Eager Strategy\r\n");
	                 }
	                 else
	                 {
	                	 System.out.println("Testing NME: normal termination with Eager Strategy");
		                 output.write("Testing NME: normal termination with Eager Strategy\r\n");
	                 }
	             }
	             if(ii == 2)
	             {
	                  parameter.pruningNodes = true;
	                  shortestPathTreeSideCost.EL = 1;
	                  if(parameter.earlyTerminate == true)
		              {
			              System.out.println("Testing KRL: k-reduction with Lazy Strategy");
			              output.write("Testing KRL: k-reduction with Lazy Strategy\r\n");
		              }
		              else
		              {
		            	  System.out.println("Testing NML: normal termination with Lazy Strategy");
			              output.write("Testing NML: normal termination with Lazy Strategy\r\n");
		              }
	             }
	                 
	             System.gc();
	             //parameter.detail = true;
	             //parameter.topks = kk;
	             System.out.println("k="+parameter.topks+" :");
	
	             output.write("k="+parameter.topks+" :\r\n");
	
	             dgraph.buildTopKPaths(source, target, parameter.topks, topKPaths.COMBINEYENEPS);
	
	
	             content=content+"  Locate top k paths="+(System.currentTimeMillis() -startTime2);
	             content=content+"  total time cost ="+((System.currentTimeMillis() -startTime2) + spttime);
	             content = content + " \r\n The threshold : " + shortestPathTreeSideCost.sideCostThreshold;
	             content = content + "\r\n";
	             //totCost1 = totCost1 + cost1;
	             System.out.println(content);
	
	             output.write(content);
	
	             // dgraph.clearForNextSPT();
	             startTime2=System.currentTimeMillis();
	             content = null;
	             System.gc();
	             content = "";
	             content=content+" Build First Shortest Path Tree="+spttime;
	             shortestPathTreeSideCost.sideCostThreshold = Integer.MAX_VALUE;
	             shortestPathTreeSideCost.totalCandidates = 0;//opt2.1
	             shortestPathTreeSideCost.rtotalCandidates = 0;//opt2.2
	             shortestPathTreeSideCost.maxThreshold = 0;
	             shortestPathTreeSideCost.sideCostThreshold=Integer.MAX_VALUE;
	         }
         }
         output.close();


        /*
        dgraph.buildTopKPaths(source, target, parameter.topks, topKPaths.COMBINEYENEPS);


        content=content+"  Locate top k paths="+(System.currentTimeMillis() -startTime2);
        content=content+"  total time cost ="+((System.currentTimeMillis() -startTime2) + spttime);
        //totCost1 = totCost1 + cost1;
        System.out.println(content);
        dgraph.clearForNextSPT();
        //System.out.println("The time cost in our algorithm is "+ totTime);
        */
    }


}
