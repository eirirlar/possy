package utility;

import java.io.IOException;

import java.io.*;
import java.text.*;
import java.util.*;
import database.database;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2007</p>
 * <p>Company: </p>
 * @author not attributable
 * @version 1.0
 */

public class result {
    static FileWriter filewriter;
    
    public static void closeResultFile() {
        try {
         if(filewriter!=null) filewriter.close();
        } catch (IOException e) {
            System.out.println("file open error");
        }

    }
}
