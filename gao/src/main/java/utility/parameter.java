package utility;

/**
 * Title:
 * Description:
 * Copyright:    Copyright (c) 2004
 * Company:
 * @author
 * @version 1.0
 */

public class parameter {
  static public String XMLDataFile="";
  static public String prefix="";
  static public int nodesSize=0;

  static public int topks=10;
  static public boolean earlyTerminate=false; //我们是否提前终止，遇到同样的长度
  static public boolean pruningNodes=false;  //我们是否采用部分节点集合进行检索
  static public boolean detail=false;  //是否输出详细路径
  public parameter() {
  }

  static public String tostring()
  {
    String result = "Dataset prefix = "+ prefix + ", topks = "+ topks;
    return result;
  }

}
