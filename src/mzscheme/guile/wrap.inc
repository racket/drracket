  case 0:
    return f(rest);
  case 1:
    return f(argv[0], rest);
  case 2:
    return f(argv[0], argv[1], rest);
  case 3:
    return f(argv[0], argv[1], argv[2], rest);
  case 4:
    return f(argv[0], argv[1], argv[2], argv[3], rest);
  case 5:
    return f(argv[0], argv[1], argv[2], argv[3], argv[4], rest);
  case 6:
    return f(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], rest);
  case 7:
    return f(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], rest);
  case 8:
    return f(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], rest);
  case 9:
    return f(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8], rest);
  case 10:
    return f(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8], argv[9], rest);
