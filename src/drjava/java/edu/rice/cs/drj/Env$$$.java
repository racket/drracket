package edu.rice.cs.drj;

// 1.2 : change to HashMap
import java.util.Hashtable;

interface EnvTags$$$ {
  static final int BOOLEAN = 0;
  static final int BYTE = BOOLEAN + 1;
  static final int CHAR = BYTE + 1;
  static final int SHORT = CHAR + 1;
  static final int INT = SHORT + 1;
  static final int LONG = INT + 1;
  static final int FLOAT = LONG + 1;
  static final int DOUBLE = FLOAT + 1;
  static final int OBJECT = DOUBLE + 1;
}

public class Env$$$ extends java.lang.ClassLoader implements EnvTags$$$ {
  public int tag;

  public boolean z;
  public byte b;
  public char c;
  public short s;
  public int i;
  public long j;
  public float f;
  public double d;
  public Object a;

  // 1.2 : change to LinkedList
  //public java.util.LinkedList defs = new java.util.LinkedList();
  public java.util.List defs = new java.util.Vector();

  public void setVal(boolean x) { z = x; tag = BOOLEAN; }
  public void setVal(byte x) { b = x; tag = BYTE; }
  public void setVal(char x) { c = x; tag = CHAR; }
  public void setVal(short x) { s = x; tag = SHORT; }
  public void setVal(int x) { i = x; tag = INT; }
  public void setVal(long x) { j = x; tag = LONG; }
  public void setVal(float x) { f = x; tag = FLOAT; }
  public void setVal(double x) { d = x; tag = DOUBLE; }
  public void setVal(Object x) { a = x; tag = OBJECT; }

  public Boolean wrap(boolean x) { return new Boolean(x); }
  public Byte wrap(byte x) { return new Byte(x); }
  public Character wrap(char x) { return new Character(x); }
  public Short wrap(short x) { return new Short(x); }
  public Integer wrap(int x) { return new Integer(x); }
  public Long wrap(long x) { return new Long(x); }
  public Float wrap(float x) { return new Float(x); }
  public Double wrap(double x) { return new Double(x); }
  public Object wrap(Object x) { return x; }

// 1.2 : change to HashMap
  public Hashtable classes = new Hashtable();

  private String dir, extensions;

  protected void setExtensionPath(String s) { extensions = s; }

  protected Env$$$(String _dir, String e) { dir = _dir; extensions = e; }

  public Class findLocalClass(String name) throws ClassNotFoundException {
    return findClass(name);
  }

  public Class findClass(String name) {
    byte[] inMemory = (byte[])classes.get(name.intern());
    if (inMemory != null) {
      return defineClass(name, inMemory, 0, inMemory.length);
    }

    try {
      java.io.FileInputStream in;
      try {
	in = new java.io.FileInputStream(dir + java.io.File.separator + name.replace('.', '/') + ".class");
      } catch (java.io.IOException e) {
	in = new java.io.FileInputStream(extensions + java.io.File.separator + name.replace('.', '/') + ".class");
      }
      int len = in.available();
      byte[] buf = new byte[len];
      in.read(buf);
      return defineClass(name, buf, 0, len);
    } catch (java.io.IOException e) {
      System.err.println("Couldn't find class: " + name);
      throw new java.lang.RuntimeException();
    }
  }

  public void addClass(String name, byte[] bytecode) {
    classes.put(name.intern(), bytecode);
  }
}
