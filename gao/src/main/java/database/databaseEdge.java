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
import java.util.*;
import utility.*;
import memory.*;

public class databaseEdge {
  Random rnd;
  public databaseEdge() {

  }

  public static void createTable(Statement sta, String prefix){
    try  {
      sta.execute("drop table "+prefix+"edge ");
    }  catch (Exception e) {
      System.out.println("no edge table");
    }

    try{
      sta.execute("create table "+prefix+"edge( "+
                  "  fromnode  char(10) not null, "+
                  "  tonode char(10) not null, "+
                  "  treeid int , "+
                  "  cost int , "+
                  "  coverroot char(10) , "+
                  "  fromnodelevel int, "+
                  "  tonodelevel int ) ");
      sta.execute("create unique index "+prefix+"edgefromto on "+prefix+"edge (fromnode, tonode)");
      sta.execute("create index "+prefix+"edgefrom on "+prefix+"edge(fromnode)");
      sta.execute("create index "+prefix+"edgeto on "+prefix+"edge(tonode)");
    }
    catch (Exception e) {
      e.printStackTrace(System.out);
    }

  }

  public static void insertIntoEdge(Statement sta, edge ed) {
    try {
      String prefix=parameter.prefix;
      ResultSet rs=sta.executeQuery("select * from "+parameter.prefix+"edge where fromnonde="+ed.fromNode.id +" and tonode="+ed.toNode.id);
      if (!rs.next())
        sta.execute("insert into "+parameter.prefix+   "edge " + "values('" + ed.fromNode.id + "','" + ed.toNode.id +
                  "' )"
                  );
    }
    catch (Exception e) {
        System.out.println("error in the insertion of the edge with "+ ed.fromNode.id+"  "+ed.toNode.id);
        database.restartConnection();

    }
  }

  public static void insertIntoEdge(Statement sta, String fromNode, String toNode, String cost) {
   try {
     //String prefix=parameter.prefix;
     //ResultSet rs=sta.executeQuery("select * from "+parameter.prefix+"edge where fromnonde="+fromNode +" and tonode="+toNode);
     //if (!rs.next())
       sta.execute("insert into "+parameter.prefix+   "edge(fromnode,tonode, cost) " + "values('" + fromNode + "','" + toNode +"',"+cost+")"
                 );
   }
   catch (Exception e) {
       System.out.println("error in the insertion of the edge with "+ fromNode+"  "+toNode);
       e.printStackTrace(System.out);
       database.restartConnection();

   }
 }


  public static Vector getToNodes(Statement sta, String fromNode){
    Vector toNodes=new Vector();

    try{
      //sta.execute("update  link1 set  mined=0;");
      ResultSet  rs=sta.executeQuery("select toNode from "+parameter.prefix+"edge where fromnode='"+fromNode+"'");
      String toNode;
      while (rs.next()){
        toNode=rs.getString("toNode");
        toNodes.add(toNode);
      }

    }catch(Exception e){
      System.out.println("error in reset the handled sign");
    }
    return toNodes;
  }

  public static void extendDataSimple(Statement sta, String fromNode, int distance, int tableID){
      String tableName="";
      if (tableID==0){
        tableName="simpleresult";
      } else if (tableID==1){
        tableName="simpleresult1";
      } else if (tableID==2){
        tableName="simpleresult2";
      }
      if (distance ==0){
        try {
          sta.execute("insert into "+ tableName +
                      " values('"+fromNode+"',0)"
              );

        }
        catch (Exception e) {
          e.printStackTrace(System.out);
        }

      }
      if (distance !=0){
        try {
         //table=0 and table =1 extend the node with the extension of out come edge
         if (tableID==0 || tableID==1){
           sta.execute(" insert into " + tableName +
                       " select distinct e1.tonode, s1.nodelevel+1 " +
                       " from " + tableName + " s1, " + parameter.prefix +
                       "edge e1 " +
                       " where s1.nodename=e1.fromnode and s1.nodelevel=" +
                       (distance - 1) +
                       " and not exists(select * from " + tableName +
                       " s2 where s2.nodename=e1.tonode)"
                       );
         }else if(tableID==2){
           sta.execute(" insert into " + tableName +
                       " select distinct e1.fromnode, s1.nodelevel+1 " +
                       " from " + tableName + " s1, " + parameter.prefix +
                       "edge e1 " +
                       " where s1.nodename=e1.tonode and s1.nodelevel=" +
                       (distance - 1) +
                       " and not exists(select * from " + tableName +
                       " s2 where s2.nodename=e1.fromnode)"
                       );

         }


        }
        catch (Exception e) {
          e.printStackTrace(System.out);
        }
      }

    }

    public static int getDistance(Statement sta, String targetNode, int tableID){
     //boolean exist=false;
     String SQLState;
     int distance =-1;
     if (tableID==0){
       try {
         SQLState = "select * from simpleresult where nodename='" + targetNode +
             "'";
         ResultSet rs = sta.executeQuery(SQLState);
         if (rs.next())
           distance = 1;  //distance is great than 1
       }
       catch (Exception e) {
         e.printStackTrace(System.out);
       }
     }else{
       try {
         SQLState = "select top 1 s1.nodelevel+s2.nodelevel as distance from simpleresult1 s1, simpleresult2 s2 where s1.nodename=s2.nodename order by distance asc";
         /*
          select top 1 s1.nodelevel+s2.nodelevel as distance
          from simpleresult1 s1, simpleresult2 s2
          where s1.nodename=s2.nodename order by distance asc
         */
         ResultSet rs = sta.executeQuery(SQLState);
         if (rs.next()){
           distance=rs.getInt("distance");
         }
       }
       catch (Exception e) {
         e.printStackTrace(System.out);
       }

     }
     return distance;

   }


   public static void clearResultSimple(Statement sta){
     try{
       //sta.execute("update  link1 set  mined=0;");
       sta.execute("delete from simpleresult;");

       //clear the simpleresult 1 and simple result 2
       sta.execute("delete from simpleresult1;");
       sta.execute("delete from simpleresult2;");

     }catch(Exception e){
       System.out.println("error in delete simpleresult");
     }
   }

   /**
   * generate the shortest path from the node id1 to the node id2
   * @param id1
   * @param id2
   */
  public static int shortPathFromDBSimpleOneDirection(Statement sta, int id1, int id2){
    String  node1, node2;
    node1 =  String.valueOf(id1);
    node2 =  String.valueOf(id2);

    //database db=new database();
    databaseEdge.clearResultSimple(sta);

    int extend=0;
    boolean stop =false;
    int distance;
    int thresh;

    while (! stop){
      databaseEdge.extendDataSimple(sta, node1,  extend, 0);

      if (databaseEdge.getDistance(sta, node2, 0)>0){
        stop=true;
        
      }else{
        extend++;
      }

    }
    return extend;

  }

  public static int shortPathFromDBSimpleTwoDirection(Statement sta, int id1, int id2){
    String  node1, node2;
    node1 =  String.valueOf(id1);
    node2 =  String.valueOf(id2);

    //database db=new database();
    databaseEdge.clearResultSimple(sta);

    int extend=0;
    boolean stop =false;
    int distance=-1;
    int thresh;

    while (! stop){
      //extend the node at two directions
      databaseEdge.extendDataSimple(sta, node1,  extend, 1);
      databaseEdge.extendDataSimple(sta, node2,  extend, 2);

      distance=databaseEdge.getDistance(sta, node2, 1);
      if (distance>0){
        stop=true;
        
      }
      extend++;

    }
    return distance;

  }


  public static int shortPathWithHop(Statement sta, int id1, int id2){
    String  node1, node2;
    node1 =  String.valueOf(id1);
    node2 =  String.valueOf(id2);

    //database db=new database();
    databaseEdge.clearResultSimple(sta);

    int extend=0;
    boolean stop =false;
    int distance=-1;
    int thresh;

    while (! stop){
      //extend the node at two directions
      databaseEdge.extendDataSimple(sta, node1,  extend, 1);
      databaseEdge.extendDataSimple(sta, node2,  extend, 2);

      distance=databaseEdge.getDistance(sta, node2, 1);
      if (distance>0){
        stop=true;
        
      }
      extend++;

    }
    return distance;

  }




  public static void setFromNodeLevel(Statement sta, String prefix, int treeid, int level){
    try{
      int update=0;
      update=sta.executeUpdate("update "+prefix+"edge set fromnodelevel="+level+", treeid="+treeid+" from "+prefix+"node node "+
                  "where "+prefix+"edge.fromnode=node.nodename and node.nodelevel="+level+" and node.treeid="+treeid);

                  //+" and "+prefix+"edge.treeid!="+treeid);
      update=sta.executeUpdate("update "+prefix+"edge set tonodelevel="+level+", treeid="+treeid+" from "+prefix+"node node "+
                  "where "+prefix+"edge.tonode=node.nodename and node.nodelevel="+level+" and node.treeid="+treeid);

                  //" and "+prefix+"edge.treeid!="+treeid);

    }catch(Exception e){
      e.printStackTrace(System.out);
    }
  }


  /**
   * copy the rest of the sub tree from the DAG into the edge table.
   * @param sta Statement
   * @param prefix String
   * @param treeid int
   */
  public static void copyFromDAG(Statement sta, String prefix, int treeid){
    try{
      int update=0;
      update=sta.executeUpdate("update "+prefix+"edge set treeid="+treeid+", fromnodelevel=dag.nodelevel-1, tonodelevel=dag.nodelevel from dag "+
                  "where "+prefix+"edge.fromnode=dag.fromnode and "+ prefix+"edge.tonode=dag.tonode");
    }catch(Exception e){
      e.printStackTrace(System.out);
    }
  }

  public static void incrementalSetLevel(Statement sta, String prefix, int treeid){
    int level=1;
    databaseEdge.setFromNodeLevel(sta, prefix, treeid, level);
    level++;
    while (databaseNode.setDAGNodeByLevel(sta, prefix, treeid, level)){
        databaseEdge.setFromNodeLevel(sta, prefix, treeid, level);
        level++;
        
    }
  }

  public static void resetTreeLevel(Statement sta, String prefix){
    try{
      sta.execute("update "+prefix+"edge set treeid=0, coverroot='', tonodelevel=0, fromnodelevel=0 where treeid>0");
    }catch(Exception e){
      e.printStackTrace(System.out);
    }
  }

  public static void addIntoDAG(Statement sta, String prefix, int treeid){
    try{
      sta.execute("insert into dag(fromnode, tonode, coverroot, nodelevel, child, treeid, tomark, distance) "+
                  " select min(fromnode), tonode, min('.'+ltrim(rtrim(fromnode))+'.'), min(fromnodelevel),0, treeid,0,0 from "+prefix+"edge "+
                  "  where treeid="+treeid+" and fromnodelevel>0 and "+"fromnodelevel<tonodelevel"+
                  "  group by tonode, treeid"
                  );
    }catch(Exception e){
      e.printStackTrace(System.out);
    }

  }

  public static void updateRandomCost(Connection connection, String prefix) {
    Random rnd = new Random(1999);
    try {
      Statement stmt = connection.createStatement(ResultSet.TYPE_FORWARD_ONLY,
                                                  ResultSet.CONCUR_UPDATABLE);
      ResultSet rs = stmt.executeQuery("select * from " + prefix + "edge");

      while (rs.next()) {
        int cost=rnd.nextInt(1999);
        if (cost==0)
          cost=1;
        rs.updateInt("tonode", cost);
        rs.updateRow();
        connection.commit();
      }
      rs.close();
      stmt.close();
    }
    catch (Exception e) {
      e.printStackTrace(System.out);
    }


  }




  public static boolean isConnect(String prefix, String fromID, String toID){
    boolean isConnect=false;
    try{
      ResultSet rs=database.sta.executeQuery("select * from "+prefix+"edge where fromnode='"+fromID +"' and tonode='"+toID+"'"+
                                              "union all "+
                                              "select * from "+prefix+"edge where fromnode='"+toID +"' and tonode='"+fromID+"'"
                                              );
      if (rs.next())
        isConnect=true;
    }catch(Exception e){
      e.printStackTrace(System.out);
    }
    return isConnect;
  }

  public static void addIntoEdge(Statement sta,String prefix, List<edge >edges){
  try{
    String SQLState=" insert into "+prefix+"edge(fromnode, tonode, cost) ";

    String data, toNode, cost;
    int pos;
    edge edge;


    for (int i=0;i<edges.size();i++){
      edge=edges.get(i);
      if (i>0){
        SQLState=SQLState+" union all ";
      }
      SQLState=SQLState+" select "+edge.fromNode.id+","+edge.toNode.id+","+edge.distance;
    }
    sta.execute(SQLState);
  }catch(Exception e){
    e.printStackTrace(System.out);
  }
 }
  public static void addIntoEdgeWithArray(Statement sta, String prefix, int [] pids, int []cids) {
    try {
      String SQLState = " insert into " + prefix +
          "edge(fromnode, tonode, cost) ";

      String data, toNode, cost;
      int pos;
      edge edge;


      for (int i = 0; i < pids.length; i++) {
        if (i > 0) {
          SQLState = SQLState + " union all ";
        }
        SQLState = SQLState + " select " + pids[i] + "," +
            cids[i] + ", 1" ;
      }
      sta.execute(SQLState);
    }
    catch (Exception e) {
      e.printStackTrace(System.out);
    }

  }

public static void clearEdge(Statement sta,String prefix){
  try{
    String SQLState=" delete from "+prefix+"edge ";
    sta.execute(SQLState);
  }catch(Exception e){
    e.printStackTrace(System.out);
  }
}





}
