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
  static public boolean earlyTerminate=false; //�����Ƿ���ǰ��ֹ������ͬ���ĳ���
  static public boolean pruningNodes=false;  //�����Ƿ���ò��ֽڵ㼯�Ͻ��м���
  static public boolean detail=false;  //�Ƿ������ϸ·��
  public parameter() {
  }

  static public String tostring()
  {
    String result = "Dataset prefix = "+ prefix + ", topks = "+ topks;
    return result;
  }

}
