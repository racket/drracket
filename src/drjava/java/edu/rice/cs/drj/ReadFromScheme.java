package edu.rice.cs.drj;

final public class ReadFromScheme extends java.io.InputStream {
  SchemeFunction func;

  protected ReadFromScheme(SchemeFunction _func) { func = _func; }

  private SchemeList Null = new SchemeList();

  public int read() {
    return func.applyInt(Null);
  }
}
