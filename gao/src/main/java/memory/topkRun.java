package memory;


import database.database;
import database.databaseNode;

import shortest.shortestPath;
import utility.*;
import java.text.SimpleDateFormat;
import java.text.DateFormat;

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
public class topkRun {
    //HashSet handled;
    int size;
    graph graph;
    int totRel = 0;

    public void run() {

        String paraFile = "parak.ini";
        parameterReader pR = new parameterReader(paraFile);
        long startTime, endTime;
        database db = new database();
        try {
            while (pR.getNextParameter() != 0) {
                System.out.println("\r\n**************Start a new data testing**************");
                size = databaseNode.getTotalNodes(database.sta);
                parameter.nodesSize = size;
                graph = new graph(size);
                graph.constructEdgsFromDB(database.sta, parameter.prefix);
                DateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

                shortestPath query = new shortestPath();

                query.TopKShortestPath(graph);
              }
//            System.out.println(" The threshold : "+shortestPathTreeSideCost.sideCostThreshold);
            System.out.println("************  End of this data set.");
              

        } catch (Exception e1) {
            e1.printStackTrace(System.out);
        }
        result.closeResultFile();
    }
}
