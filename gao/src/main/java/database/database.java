package database;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2007</p>
 * <p>Company: </p>
 * @author not attributable
 * @version 1.0
 */
import java.io.*;
import java.sql.*;

import utility.*;
import database.*;
import shortest.*;
import memory.*;

//Q: connect to database, setup necessary tables in db:
public class database {

    float threshold;

    static String sql_driver = "com.microsoft.jdbc.sqlserver.SQLServerDriver";
    //static String sql_driver="COM.ibm.db2.jdbc.app.DB2Driver";
    //static String sql_url = "jdbc:microsoft:sqlserver://192.9.200.41;DatabaseName=graph";
     static String  sql_url="jdbc:microsoft:sqlserver://localhost:1433;DatabaseName=graph";
    //static String  sql_url="jdbc:db2:graph";//


    static String user = "sa";
 //    static String   user="db2admin";
    //for 31
    static String pwd = "1637"; //访问数据库的密码 server41: pwd="1638";
 //  static String   pwd="123";

    public static Connection dbcon = null;
    public static Statement sta;
    public static Statement updateSta;


    public database() {
        if (dbcon == null) {
            createConnection();
        }
    }


    public static void createConnection() {
        try {
            Class.forName(sql_driver); //加载驱动
            System.out.println("driver is ok");
            dbcon = DriverManager.getConnection(sql_url, user, pwd);
            sta = dbcon.createStatement();
            updateSta = dbcon.createStatement(ResultSet.TYPE_FORWARD_ONLY,
                                              ResultSet.CONCUR_UPDATABLE);
        } catch (Exception e) {
            e.printStackTrace(); //printStackTrace();
        }
    }


    public static void InitGraphTablesFromFile(String fileName) {
        String XPathFile = parameter.XMLDataFile;
        File file = new File(XPathFile);

        try {
            // Create an BufferedReader so we can read a line at the time.
            BufferedReader reader = new BufferedReader(new FileReader(XPathFile));
            String inLine = reader.readLine();
            int lineCount = 0;
            while (inLine != null) {
                if (inLine.length() > 0 &&
                    inLine.charAt(inLine.length() - 1) != '*') { //our system does not deal with the * in the last
                    database.handleOneEdge(sta, inLine);
                }
                inLine = reader.readLine();
                lineCount++;

                //System.out.println("the line count is "+ lineCount);
            }
        } catch (IOException e) {
            e.printStackTrace(System.out);
            System.out.println("file open error");
        }
    }


    private static void handleOneEdge(Statement sta, String line) {
        String first, second;
        node firstNode = null, secondNode = null;
        int begin, end;
        edge currentedge;
        if (line.indexOf("<edge source=") > 0) {
            begin = line.indexOf("\"", 0);

            end = line.indexOf("\"", begin + 1);
            first = line.substring(begin + 1, end);

            begin = line.indexOf("\"", end + 1);
            end = line.indexOf("\"", begin + 1);
            second = line.substring(begin + 1, end);

            //databaseEdge.insertIntoEdge(sta,first, second);
            //databaseNode.insertIntoNode(sta, first);
            //databaseNode.insertIntoNode(sta, second);
        }
    }

    public static void clearGraphTables(Statement sta) {
        try {
            sta.execute("truncate table  " + parameter.prefix + "node");
            sta.execute("truncate table  " + parameter.prefix + "edge");
           // sta.execute("truncate table  " + parameter.prefix + "nodecoding");
            //sta.execute("delete from tree");
        } catch (Exception e) {
            e.printStackTrace(System.out);
        }

    }

    public static void createTmpTable(Statement sta) {
        try {
            //sta.execute("Drop  TABLE simpleResult");
            sta.execute(
                    "CREATE TABLE simpleResult (nodename char(10), nodelevel int) ");
        } catch (Exception e) {
            System.out.println("table existing");
        }

        try {
            sta.execute(
                    "CREATE TABLE simpleResult1 (nodename char(10), nodelevel int) ");
            sta.execute(
                    "CREATE TABLE simpleResult2 (nodename char(10), nodelevel int) ");
            sta.execute(
                    "CREATE TABLE lownode (nodename char(10) , nodelevel int)");
            sta.execute(
                    "CREATE TABLE topnode (nodename char(10) , nodelevel int)");
            //sta.execute("delete from tree");
        } catch (Exception e) {
            System.out.println("table existing");
        }

    }

    public static void restartConnection() {
        try {
            sta.close();
            sta = null;
        } catch (Exception e) {
            System.out.println("connect close error");
        }

        database.createConnection();

    }
    public static void closeConnection()
    {
         try {
             sta.close();
         }catch (Exception e) {
            System.out.println("connect close error");
        }
    }
    /**
     * build the node table with the prefix
     * @param prefix
     */
    public static void initGraphTable(String prefix) {
        database.createTmpTable(sta);
        databaseEdge.createTable(sta, prefix);
        databaseNode.createTable(sta, prefix);
    }


}
