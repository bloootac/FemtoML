
package femtoml
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 31 => 10;  case 32 => 11;  case 33 => 12;  case 34 => 13;  case 41 => 14;  }
  case 4 => { case 32 => 18;  case 33 => 12;  case 34 => 13;  }
  case 7 => { case 41 => 23;  }
  case 12 => { case 34 => 28;  }
  case 15 => { case 32 => 32;  case 33 => 12;  case 34 => 13;  }
  case 16 => { case 37 => 38;  case 38 => 39;  case 39 => 40;  case 40 => 41;  }
  case 21 => { case 35 => 50;  case 36 => 51;  case 43 => 52;  case 44 => 53;  }
  case 25 => { case 53 => 57;  case 54 => 58;  }
  case 27 => { case 31 => 59;  case 32 => 11;  case 33 => 12;  case 34 => 13;  case 41 => 14;  }
  case 29 => { case 41 => 60;  }
  case 30 => { case 31 => 61;  case 32 => 11;  case 33 => 12;  case 34 => 13;  case 41 => 14;  }
  case 31 => { case 41 => 62;  }
  case 33 => { case 40 => 64;  case 51 => 65;  case 52 => 66;  }
  case 34 => { case 37 => 67;  case 38 => 39;  case 39 => 40;  case 40 => 41;  }
  case 35 => { case 49 => 69;  case 50 => 70;  }
  case 44 => { case 32 => 73;  case 33 => 12;  case 34 => 13;  }
  case 48 => { case 35 => 74;  }
  case 54 => { case 32 => 79;  case 33 => 12;  case 34 => 13;  }
  case 55 => { case 32 => 80;  case 33 => 12;  case 34 => 13;  }
  case 65 => { case 40 => 83;  }
  case 71 => { case 38 => 87;  case 39 => 40;  case 40 => 41;  }
  case 72 => { case 32 => 88;  case 33 => 12;  case 34 => 13;  case 45 => 89;  case 46 => 90;  }
  case 76 => { case 32 => 93;  case 33 => 12;  case 34 => 13;  }
  case 77 => { case 35 => 50;  case 36 => 94;  }
  case 82 => { case 42 => 96;  case 55 => 97;  case 56 => 98;  }
  case 86 => { case 37 => 100;  case 38 => 39;  case 39 => 40;  case 40 => 41;  }
  case 95 => { case 40 => 104;  case 57 => 105;  case 58 => 106;  }
  case 101 => { case 32 => 108;  case 33 => 12;  case 34 => 13;  }
  case 103 => { case 35 => 110;  case 47 => 111;  case 48 => 112;  }
  case 105 => { case 40 => 113;  }
  case 107 => { case 42 => 114;  }
  case 115 => { case 35 => 117;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case 3 => SHIFT(1);  case 4 => SHIFT(2);  case 5 => SHIFT(3);  case 6 => SHIFT(4);  case 10 => SHIFT(5);  case 12 => SHIFT(6);  case 19 => SHIFT(7);  case 22 => SHIFT(8);  case 26 => SHIFT(9);  case _ => REDUCE(31,1,0);  }
  case 1 => { case _ => REDUCE(34,10,1);  }
  case 2 => { case _ => REDUCE(34,11,1);  }
  case 3 => { case 17 => SHIFT(15);  case 18 => SHIFT(16);  case _ => REDUCE(34,12,1);  }
  case 4 => { case 3 => SHIFT(1);  case 4 => SHIFT(2);  case 5 => SHIFT(17);  case 6 => SHIFT(4);  case 10 => SHIFT(5);  case 12 => SHIFT(6);  case 19 => SHIFT(7);  case _ => ERROR;  }
  case 5 => { case 5 => SHIFT(19);  case _ => ERROR;  }
  case 6 => { case 5 => SHIFT(20);  case 6 => SHIFT(21);  case _ => ERROR;  }
  case 7 => { case 5 => SHIFT(22);  case 22 => SHIFT(8);  case 26 => SHIFT(9);  case _ => ERROR;  }
  case 8 => { case 5 => SHIFT(24);  case _ => ERROR;  }
  case 9 => { case 5 => SHIFT(25);  case _ => ERROR;  }
  case 10 => { case 0 => SHIFT(26);  case _ => ERROR;  }
  case 11 => { case 27 => SHIFT(27);  case _ => ERROR;  }
  case 12 => { case 3 => SHIFT(1);  case 4 => SHIFT(2);  case 5 => SHIFT(17);  case 6 => SHIFT(4);  case 10 => SHIFT(5);  case _ => REDUCE(32,4,1);  }
  case 13 => { case _ => REDUCE(33,9,1);  }
  case 14 => { case 21 => SHIFT(29);  case 27 => SHIFT(30);  case 28 => SHIFT(31);  case _ => ERROR;  }
  case 15 => { case 3 => SHIFT(1);  case 4 => SHIFT(2);  case 5 => SHIFT(17);  case 6 => SHIFT(4);  case 10 => SHIFT(5);  case 12 => SHIFT(6);  case 19 => SHIFT(7);  case _ => ERROR;  }
  case 16 => { case 5 => SHIFT(33);  case 6 => SHIFT(34);  case 16 => SHIFT(35);  case 23 => SHIFT(36);  case 24 => SHIFT(37);  case _ => ERROR;  }
  case 17 => { case _ => REDUCE(34,12,1);  }
  case 18 => { case 7 => SHIFT(42);  case _ => ERROR;  }
  case 19 => { case 14 => SHIFT(43);  case _ => ERROR;  }
  case 20 => { case 13 => SHIFT(44);  case _ => ERROR;  }
  case 21 => { case 3 => SHIFT(45);  case 4 => SHIFT(46);  case 5 => SHIFT(47);  case 6 => SHIFT(48);  case 10 => SHIFT(49);  case _ => REDUCE(44,40,0);  }
  case 22 => { case 17 => SHIFT(15);  case 18 => SHIFT(16);  case _ => ERROR;  }
  case 23 => { case 20 => SHIFT(54);  case 21 => SHIFT(29);  case 28 => SHIFT(31);  case _ => ERROR;  }
  case 24 => { case 17 => SHIFT(55);  case _ => ERROR;  }
  case 25 => { case 5 => SHIFT(56);  case _ => REDUCE(54,59,0);  }
  case 26 => { case _ => ACCEPT;  }
  case 27 => { case 3 => SHIFT(1);  case 4 => SHIFT(2);  case 5 => SHIFT(3);  case 6 => SHIFT(4);  case 10 => SHIFT(5);  case 12 => SHIFT(6);  case 19 => SHIFT(7);  case 22 => SHIFT(8);  case 26 => SHIFT(9);  case _ => REDUCE(31,1,0);  }
  case 28 => { case _ => REDUCE(33,8,2);  }
  case 29 => { case 5 => SHIFT(22);  case 22 => SHIFT(8);  case 26 => SHIFT(9);  case _ => ERROR;  }
  case 30 => { case 3 => SHIFT(1);  case 4 => SHIFT(2);  case 5 => SHIFT(3);  case 6 => SHIFT(4);  case 10 => SHIFT(5);  case 12 => SHIFT(6);  case 19 => SHIFT(7);  case 22 => SHIFT(8);  case 26 => SHIFT(9);  case _ => REDUCE(31,1,0);  }
  case 31 => { case 5 => SHIFT(22);  case 22 => SHIFT(8);  case 26 => SHIFT(9);  case _ => ERROR;  }
  case 32 => { case _ => REDUCE(41,31,3);  }
  case 33 => { case 5 => SHIFT(63);  case 6 => SHIFT(34);  case 23 => SHIFT(36);  case 24 => SHIFT(37);  case _ => REDUCE(40,29,1);  }
  case 34 => { case 5 => SHIFT(33);  case 6 => SHIFT(34);  case 16 => SHIFT(35);  case 23 => SHIFT(36);  case 24 => SHIFT(37);  case _ => ERROR;  }
  case 35 => { case 5 => SHIFT(68);  case _ => REDUCE(50,52,0);  }
  case 36 => { case _ => REDUCE(40,27,1);  }
  case 37 => { case _ => REDUCE(40,28,1);  }
  case 38 => { case _ => REDUCE(41,35,3);  }
  case 39 => { case _ => REDUCE(37,21,1);  }
  case 40 => { case 29 => SHIFT(71);  case _ => REDUCE(38,24,1);  }
  case 41 => { case _ => REDUCE(39,26,1);  }
  case 42 => { case _ => REDUCE(34,14,3);  }
  case 43 => { case 8 => SHIFT(72);  case _ => ERROR;  }
  case 44 => { case 3 => SHIFT(1);  case 4 => SHIFT(2);  case 5 => SHIFT(17);  case 6 => SHIFT(4);  case 10 => SHIFT(5);  case 12 => SHIFT(6);  case 19 => SHIFT(7);  case _ => ERROR;  }
  case 45 => { case _ => REDUCE(35,15,1);  }
  case 46 => { case _ => REDUCE(35,16,1);  }
  case 47 => { case _ => REDUCE(35,17,1);  }
  case 48 => { case 3 => SHIFT(45);  case 4 => SHIFT(46);  case 5 => SHIFT(47);  case 6 => SHIFT(48);  case 10 => SHIFT(49);  case _ => ERROR;  }
  case 49 => { case 5 => SHIFT(75);  case _ => ERROR;  }
  case 50 => { case 29 => SHIFT(76);  case _ => ERROR;  }
  case 51 => { case _ => REDUCE(43,38,1);  }
  case 52 => { case 15 => SHIFT(77);  case _ => REDUCE(44,41,1);  }
  case 53 => { case 7 => SHIFT(78);  case _ => ERROR;  }
  case 54 => { case 3 => SHIFT(1);  case 4 => SHIFT(2);  case 5 => SHIFT(17);  case 6 => SHIFT(4);  case 10 => SHIFT(5);  case 12 => SHIFT(6);  case 19 => SHIFT(7);  case _ => ERROR;  }
  case 55 => { case 3 => SHIFT(1);  case 4 => SHIFT(2);  case 5 => SHIFT(17);  case 6 => SHIFT(4);  case 10 => SHIFT(5);  case 12 => SHIFT(6);  case 19 => SHIFT(7);  case _ => ERROR;  }
  case 56 => { case _ => REDUCE(53,57,1);  }
  case 57 => { case 5 => SHIFT(81);  case _ => REDUCE(54,60,1);  }
  case 58 => { case 17 => SHIFT(82);  case _ => ERROR;  }
  case 59 => { case _ => REDUCE(31,2,3);  }
  case 60 => { case _ => REDUCE(41,32,3);  }
  case 61 => { case _ => REDUCE(31,3,3);  }
  case 62 => { case _ => REDUCE(41,33,3);  }
  case 63 => { case _ => REDUCE(40,29,1);  }
  case 64 => { case _ => REDUCE(51,54,1);  }
  case 65 => { case 5 => SHIFT(63);  case 6 => SHIFT(34);  case 23 => SHIFT(36);  case 24 => SHIFT(37);  case _ => REDUCE(52,56,1);  }
  case 66 => { case _ => REDUCE(39,25,2);  }
  case 67 => { case 7 => SHIFT(84);  case _ => ERROR;  }
  case 68 => { case _ => REDUCE(49,50,1);  }
  case 69 => { case 14 => SHIFT(85);  case _ => REDUCE(50,53,1);  }
  case 70 => { case 13 => SHIFT(86);  case _ => ERROR;  }
  case 71 => { case 5 => SHIFT(33);  case 6 => SHIFT(34);  case 23 => SHIFT(36);  case 24 => SHIFT(37);  case _ => ERROR;  }
  case 72 => { case 3 => SHIFT(1);  case 4 => SHIFT(2);  case 5 => SHIFT(17);  case 6 => SHIFT(4);  case 10 => SHIFT(5);  case 12 => SHIFT(6);  case 19 => SHIFT(7);  case _ => REDUCE(46,44,0);  }
  case 73 => { case _ => REDUCE(32,5,4);  }
  case 74 => { case 7 => SHIFT(91);  case _ => ERROR;  }
  case 75 => { case 14 => SHIFT(92);  case _ => ERROR;  }
  case 76 => { case 3 => SHIFT(1);  case 4 => SHIFT(2);  case 5 => SHIFT(17);  case 6 => SHIFT(4);  case 10 => SHIFT(5);  case 12 => SHIFT(6);  case 19 => SHIFT(7);  case _ => ERROR;  }
  case 77 => { case 3 => SHIFT(45);  case 4 => SHIFT(46);  case 5 => SHIFT(47);  case 6 => SHIFT(48);  case 10 => SHIFT(49);  case _ => ERROR;  }
  case 78 => { case _ => REDUCE(32,6,4);  }
  case 79 => { case _ => REDUCE(32,7,4);  }
  case 80 => { case _ => REDUCE(41,34,4);  }
  case 81 => { case _ => REDUCE(53,58,2);  }
  case 82 => { case 5 => SHIFT(95);  case _ => ERROR;  }
  case 83 => { case _ => REDUCE(51,55,2);  }
  case 84 => { case _ => REDUCE(40,30,3);  }
  case 85 => { case 5 => SHIFT(99);  case _ => ERROR;  }
  case 86 => { case 5 => SHIFT(33);  case 6 => SHIFT(34);  case 16 => SHIFT(35);  case 23 => SHIFT(36);  case 24 => SHIFT(37);  case _ => ERROR;  }
  case 87 => { case _ => REDUCE(38,23,3);  }
  case 88 => { case _ => REDUCE(45,42,1);  }
  case 89 => { case 14 => SHIFT(101);  case _ => REDUCE(46,45,1);  }
  case 90 => { case 9 => SHIFT(102);  case _ => ERROR;  }
  case 91 => { case _ => REDUCE(35,19,3);  }
  case 92 => { case 8 => SHIFT(103);  case _ => ERROR;  }
  case 93 => { case _ => REDUCE(36,20,3);  }
  case 94 => { case _ => REDUCE(43,39,3);  }
  case 95 => { case 5 => SHIFT(63);  case 6 => SHIFT(34);  case 23 => SHIFT(36);  case 24 => SHIFT(37);  case _ => REDUCE(58,66,0);  }
  case 96 => { case _ => REDUCE(55,61,1);  }
  case 97 => { case 15 => SHIFT(107);  case _ => REDUCE(56,63,1);  }
  case 98 => { case _ => REDUCE(41,36,5);  }
  case 99 => { case _ => REDUCE(49,51,3);  }
  case 100 => { case _ => REDUCE(37,22,4);  }
  case 101 => { case 3 => SHIFT(1);  case 4 => SHIFT(2);  case 5 => SHIFT(17);  case 6 => SHIFT(4);  case 10 => SHIFT(5);  case 12 => SHIFT(6);  case 19 => SHIFT(7);  case _ => ERROR;  }
  case 102 => { case 11 => SHIFT(109);  case _ => ERROR;  }
  case 103 => { case 3 => SHIFT(45);  case 4 => SHIFT(46);  case 5 => SHIFT(47);  case 6 => SHIFT(48);  case 10 => SHIFT(49);  case _ => REDUCE(48,48,0);  }
  case 104 => { case _ => REDUCE(57,64,1);  }
  case 105 => { case 5 => SHIFT(63);  case 6 => SHIFT(34);  case 23 => SHIFT(36);  case 24 => SHIFT(37);  case _ => REDUCE(58,67,1);  }
  case 106 => { case _ => REDUCE(42,37,2);  }
  case 107 => { case 5 => SHIFT(95);  case _ => ERROR;  }
  case 108 => { case _ => REDUCE(45,43,3);  }
  case 109 => { case _ => REDUCE(34,13,7);  }
  case 110 => { case _ => REDUCE(47,46,1);  }
  case 111 => { case 14 => SHIFT(115);  case _ => REDUCE(48,49,1);  }
  case 112 => { case 9 => SHIFT(116);  case _ => ERROR;  }
  case 113 => { case _ => REDUCE(57,65,2);  }
  case 114 => { case _ => REDUCE(55,62,3);  }
  case 115 => { case 3 => SHIFT(45);  case 4 => SHIFT(46);  case 5 => SHIFT(47);  case 6 => SHIFT(48);  case 10 => SHIFT(49);  case _ => ERROR;  }
  case 116 => { case 11 => SHIFT(118);  case _ => ERROR;  }
  case 117 => { case _ => REDUCE(47,47,3);  }
  case 118 => { case _ => REDUCE(35,18,7);  }
  case _ => { case _ => ERROR }
  }
}
