package edu.rice.cs.drj;

public final class SchemeList extends SchemeValue {
  public static SchemeList Null = new SchemeList();

  // null
  public SchemeList() { init(); }

  // cons
  public SchemeList(boolean car, SchemeList cdr) { init(car, cdr); }
  public SchemeList(byte car, SchemeList cdr) { init(car, cdr); }
  public SchemeList(char car, SchemeList cdr) { init(car, cdr); }
  public SchemeList(short car, SchemeList cdr) { init(car, cdr); }
  public SchemeList(int car, SchemeList cdr) { init(car, cdr); }
  public SchemeList(long car, SchemeList cdr) { init(car, cdr); }
  public SchemeList(float car, SchemeList cdr) { init(car, cdr); }
  public SchemeList(double car, SchemeList cdr) { init(car, cdr); }
  public SchemeList(Object car, SchemeList cdr) { init(car, cdr); }

  private native void init();
  private native void init(boolean car, SchemeList cdr);
  private native void init(byte car, SchemeList cdr);
  private native void init(char car, SchemeList cdr);
  private native void init(short car, SchemeList cdr);
  private native void init(int car, SchemeList cdr);
  private native void init(long car, SchemeList cdr);
  private native void init(float car, SchemeList cdr);
  private native void init(double car, SchemeList cdr);
  private native void init(Object car, SchemeList cdr);
}
