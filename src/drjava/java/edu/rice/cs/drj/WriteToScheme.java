package edu.rice.cs.drj;

final public class WriteToScheme extends java.io.OutputStream {
  SchemeFunction func;

  protected WriteToScheme(SchemeFunction _func) { func = _func; }

  private SchemeList Null = new SchemeList();

  public synchronized void write(int b) {
    func.applyVoid(new SchemeList((byte) b, Null));
  }
}
