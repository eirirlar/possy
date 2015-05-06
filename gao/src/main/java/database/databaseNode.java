package database;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2007</p>
 * <p>Company: </p>
 * @author not attributable
 * @version 1.0
 */

import java.sql.*;
import memory.*;
import utility.*;


public class databaseNode {
  public databaseNode() {
  }

  public static void createTable(Statement sta, String prefix){

     try  {
       sta.execute("drop table "+prefix+"node ");
     }  catch (Exception e) {
      System.out.println("no node table");
     }

     try{

       sta.execute("create table "+prefix+"node( "+
                   "  nodename  int not null, "+
                   "  indegree int,  "+
                   "  outdegree int,  "+
                   "  treeid int,  "+
                   "  nodelevel int,  "+
                   "  coverroot char(10),  "+
                   "  handled int) ");

       sta.execute("create unique index "+prefix+"nodename on "+prefix+"node (nodename)");

      }
      catch (Exception e) {
       e.printStackTrace(System.out);
     }

   }


  public static void clearHandledSing(Statement sta, String prefix){
      try{
        //sta.execute("update  link1 set  mined=0;");
        sta.execute("update "+prefix+"node set handled=0;");

      }catch(Exception e){
        System.out.println("error in reset the handled sign");
      }
  }

  public static String getNextNode(Statement sta){
    String nodeName="";

    try{
      //sta.execute("update  link1 set  mined=0;");
      ResultSet  rs=sta.executeQuery("select top 1 nodename from "+parameter.prefix+"node where handled=0;");
      if (rs.next()){
        nodeName=rs.getString("nodename");
        sta.execute("update "+parameter.prefix+"node set handled=1 where nodename='"+nodeName+"'");
      }

    }catch(Exception e){
      e.printStackTrace(System.out);
      //System.out.println("error in reset the handled sign");
    }
    return nodeName;

  }

  public static int getTotalNodes (Statement sta){
    int tot = 0, tot1 =0;

    try {
      //sta.execute("update  link1 set  mined=0;");
      ResultSet rs = sta.executeQuery("select count(*) as cnt from "+parameter.prefix+"node;");
      if (rs.next()) {
        tot = rs.getInt("cnt");
      }
      rs = sta.executeQuery("select max(nodename) as cnt from "+parameter.prefix+"node;");
      if (rs.next()) {
         tot1 = rs.getInt("cnt")+1;
      }
      if (tot1>tot)
         tot=tot1;

    }
    catch (Exception e) {
      System.out.println("error in getTotalNodes reset the handled sign");
    }
    return tot;
  }

  public static void insertIntoNode(Statement sta, node n1){
      try {

        ResultSet rs=sta.executeQuery("select * from "+parameter.prefix+"node where nodename='"+n1.id+"'");
        if (! rs.next())
          sta.execute("insert into "+parameter.prefix+"node(nodename,handled)  values ('"+n1.id+"',0) " );
      }
     catch (Exception e) {
        database.restartConnection();
     }
  }

  public static void insertIntoNode(Statement sta, String nodeName){
    try {
     ResultSet rs=sta.executeQuery("select * from "+parameter.prefix+"node where nodename='"+nodeName+"'");
     if (! rs.next())
       sta.execute("insert into "+parameter.prefix+"node(nodename,handled)  values ('"+nodeName+"',0) " );
   }
   catch (Exception e) {
     database.restartConnection();
  }
 }


  public static void setHandled(Statement sta, String prefix, String nodename){
    try{
      sta.execute("update "+prefix+"node set handled=1 where nodename="+nodename);
    }catch(Exception e){
      e.printStackTrace(System.out);
    }
  }

  public static void resetAllHandled(Statement sta, String prefix){
    try{
        sta.execute("update "+prefix+"node set handled=0");
     }catch(Exception e){
        e.printStackTrace(System.out);
     }

  }
  //set the dag name
  public static void setDAGRoot(Statement sta, String prefix, int treeid, String nodeName){
    try{
      sta.execute("update "+prefix+"node set treeid="+treeid+", nodelevel=1 where nodename="+nodeName);
     }catch(Exception e){
        e.printStackTrace(System.out);
     }

  }

  public static void copyFromDAG(Statement sta, String prefix, int treeid, String nodeName){
    try{
      sta.execute("update "+prefix+"node set treeid="+treeid+", nodelevel=dag.nodelevel from dag where dag.tonode=nodename ");
     }catch(Exception e){
        e.printStackTrace(System.out);
     }

  }


  public static boolean setDAGNodeByLevel(Statement sta, String prefix, int treeid, int level){
    int update=0;
    try{
       update=sta.executeUpdate("update "+prefix+"node set treeid="+treeid+", nodelevel="+level+" from "+prefix+"edge "+
                        "where "+prefix+"node.nodelevel=0 and "+prefix+"edge.fromnodelevel= "+(level-1)+
                        "and "+prefix+"edge.tonode="+prefix+"node.nodename"
                      );
       }catch(Exception e){
          e.printStackTrace(System.out);
       }
    return (update>0);
  }

  public static void resetTreeLevel(Statement sta, String prefix){
    try{
      sta.execute("update "+prefix+"node set treeid=0, nodelevel=0 where treeid>0");
    }catch(Exception e){
      e.printStackTrace(System.out);
    }
  }

  public static void initNodes(Statement sta, int tot){
    try{
      sta.execute("truncate table nodeT ");
      for (int i = 1; i < tot+1; i++) {
         sta.execute("insert into nodeT (nodename, partid) values("+i+",0"+")");
      }
    }catch(Exception e){
      e.printStackTrace(System.out);
    }
  }

  public static String getNextPartitionRoot(Statement sta){
    String nextNode="";
    try{
      ResultSet rs=sta.executeQuery("select top 1 nodename from nodeT where partid=0 order by (ingraph+outgraph) desc ");
      if (rs.next()){
        nextNode=rs.getString("nodeName");
      }

    }catch(Exception e){
      e.printStackTrace(System.out);
    }
    return nextNode;
  }

  public static void setInandOutDegree(Statement sta){
    try{
       sta.execute("update nodet set ingraph=(select count(*) from edget where tonode=nodet.nodename)");
       sta.execute("update nodet set outgraph=(select count(*) from edget where fromnode=nodet.nodename)");
    }catch(Exception e){
      e.printStackTrace(System.out);
    }
  }




  public static int [] getChildNode(String prefix, String fromID){
    int child[]=new int[100];
    int nextID;
    int size=1;
    try{
      ResultSet rs=database.sta.executeQuery("select tonode from "+prefix+"edge where handled=0 and fromnode="+fromID);

      while (rs.next()){
        nextID=rs.getInt("tonode");
        child[size]=nextID;
        size++;
      }
      child[0]=size-1;
   }catch(Exception e){
     e.printStackTrace(System.out);
   }
   return child;

  }


}
