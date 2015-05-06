package utility;

/**
 * Title:
 * Description:
 * Copyright:    Copyright (c) 2004
 * Company:
 * @author
 * @version 1.0
 */

/**
 * Title:
 * Description:  read the parameter and write the result
 * Copyright:    Copyright (c) 2004
 * Company:
 * @author
 * @version 1.0
 */
import java.util.*;

import java.io.*;
import database.database;
import database.databaseNode;

public class parameterReader {

    static Properties iniProp = new Properties();
    String iniFile;
    Vector paras = new Vector();
    int group = 0;
    public parameterReader(String iniFile1) {
        this.iniFile = iniFile1;
        initParas();
    }

    public void initParas() {
        try {
            BufferedReader reader = new BufferedReader(new FileReader(iniFile));
            String inLine = reader.readLine();
            while (inLine != null) {
                paras.add(inLine);
                inLine = reader.readLine();
            }
        } catch (IOException e) {
        	
            System.out.println("file open error");
        }
    }

    public void reset() {
        group = 0;
    }

    public void increase() {
        group++;
    }

    public String getValue(String key) {
        String values;
        int idx = 0;
        for (int i = 0; i < paras.size(); i++) {
            values = (String) paras.elementAt(i);
            if (values.indexOf(key) != -1) {
                if (idx == group) {
                    int pos = values.indexOf("=");
                    return values.substring(pos + 1).trim();
                } else {
                    idx++;
                }
            }
        }
        return null;
    }

    public int getNextParameter() {
        String prefix=" ";
            parameter.XMLDataFile = this.getValue("XMLDataFile");
            if (parameter.XMLDataFile == null) {
                return 0;
            }
            prefix  =  this.getValue("prefix");
            parameter.prefix = prefix;

            parameter.topks = Integer.valueOf(this.getValue("topks")).
                               intValue();
            String detail= this.getValue("detail");
            if (detail.equalsIgnoreCase("0")){
                parameter.detail = false;
            }else{
                parameter.detail = true;
            }

            System.out.println(parameter.tostring() + " being processed ");
            this.increase();
        //}
        return 1;
    }

}
