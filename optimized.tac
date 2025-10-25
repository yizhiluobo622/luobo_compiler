TAC Program
Global Variables:
  N: IntType = 123
Global Array Variables:
  A: ArrayType { element_type: ArrayType { element_type: IntType, array_size: Constant("N") }, array_size: Constant("N") } 维度: [123, 123]
  B: ArrayType { element_type: ArrayType { element_type: IntType, array_size: Constant("N") }, array_size: Constant("N") } 维度: [123, 123]
  C: ArrayType { element_type: ArrayType { element_type: IntType, array_size: Constant("N") }, array_size: Constant("N") } 维度: [123, 123]
Functions:
  Function 0:
Function: mm
Return Type: VoidType
Parameters:
  n: IntType
  A: ArrayType { element_type: ArrayType { element_type: IntType, array_size: Constant("N") }, array_size: Unspecified }
  B: ArrayType { element_type: ArrayType { element_type: IntType, array_size: Constant("N") }, array_size: Unspecified }
  C: ArrayType { element_type: ArrayType { element_type: IntType, array_size: Constant("N") }, array_size: Unspecified }
Basic Blocks:
  Basic Block 0:
L0:
  前驱: [3, 7], 后继: []
  Basic Block 1:
L1:
  0: i = 0
  1: k = 0
  2: j = 0
  3: i = 0
  4: j = 0
  5: L0:

  Basic Block 2:
L2:
  前驱: [3], 后继: []
  Basic Block 3:
L3:
  0: t0 = i < n
  1: if t0 goto L1 else goto L2
  2: j = 0
  3: L3:
  前驱: [5], 后继: [1, 2]
  Basic Block 4:
L4:
  前驱: [5], 后继: []
  Basic Block 5:
L5:
  0: t1 = j < n
  1: if t1 goto L4 else goto L5
  2: t2 = getelementptr C, [i, j]
  3: store 0 to t2
  4: t3 = j + 1
  5: j = t3
  6: goto L3
  7: L5:
  前驱: [5], 后继: [4, 5, 3]
  Basic Block 6:
L6:
  前驱: [12, 24], 后继: [1]
  Basic Block 7:
L7:
  0: t4 = i + 1
  1: i = t4
  2: goto L0
  3: L2:

  Basic Block 8:
L8:
  前驱: [12, 16, 22], 后继: []
  Basic Block 9:
L9:
  0: i = 0
  1: j = 0
  2: k = 0
  3: L6:

  Basic Block 10:
L10:
  前驱: [16], 后继: []
  Basic Block 11:
L11:
  0: return
  前驱: [16], 后继: []
  Basic Block 12:
L12:
  0: t5 = k < n
  1: if t5 goto L7 else goto L8
  2: i = 0
  3: L9:
  前驱: [16], 后继: [7, 9]
  Basic Block 13:
L13:
  前驱: [16], 后继: []
  Basic Block 14:
L14:
  前驱: [16], 后继: []
  Basic Block 15:
L15:
  前驱: [20], 后继: []
  Basic Block 16:
L16:
  0: t6 = i < n
  1: if t6 goto L10 else goto L11
  2: t8 = getelementptr A, [i, k]
  3: t7 = load t8
  4: t9 = t7 == 0
  5: if t9 goto L12 else goto L13
  6: t10 = i + 1
  7: i = t10
  8: goto L9
  9: goto L14
  10: L14:
  前驱: [20], 后继: [10, 11, 12, 13, 9, 14]
  Basic Block 17:
L17:
  前驱: [20], 后继: []
  Basic Block 18:
L18:
  0: j = 0
  1: L15:

  Basic Block 19:
L19:

  Basic Block 20:
L20:
  0: t11 = j < n
  1: if t11 goto L16 else goto L17
  2: t13 = getelementptr C, [i, j]
  3: t12 = load t13
  4: t15 = getelementptr A, [i, k]
  5: t14 = load t15
  6: t17 = getelementptr B, [k, j]
  7: t16 = load t17
  8: t18 = t14 * t16
  9: t19 = t12 + t18
  10: t20 = getelementptr C, [i, j]
  11: store t19 to t20
  12: t21 = j + 1
  13: j = t21
  14: goto L15
  15: L17:
  前驱: [], 后继: [16, 18, 15]
  Basic Block 21:
L21:
  前驱: [], 后继: [9]
  Basic Block 22:
L22:
  0: t22 = i + 1
  1: i = t22
  2: goto L9
  3: L11:

  Basic Block 23:
L23:
  前驱: [], 后继: [7]
  Basic Block 24:
L24:
  0: t23 = k + 1
  1: k = t23
  2: goto L6
  3: L8:


  Function 1:
Function: main
Return Type: IntType
Parameters:
Basic Blocks:
  Basic Block 0:
L0:
  前驱: [2, 4], 后继: []
  Basic Block 1:
L1:
  0: t0 = call getint()
  1: n = t0
  2: i = 0
  3: j = 0
  4: i = 0
  5: j = 0
  6: L0:

  Basic Block 2:
L2:
  0: t1 = i < n
  1: if t1 goto L1 else goto L2
  2: j = 0
  3: L3:
  前驱: [2], 后继: [1, 2]
  Basic Block 3:
L3:
  0: t2 = j < n
  1: if t2 goto L4 else goto L5
  2: t3 = call getint()
  3: t4 = getelementptr A, [i, j]
  4: store t3 to t4
  5: t5 = j + 1
  6: j = t5
  7: goto L3
  8: L5:
  前驱: [3], 后继: [4, 5, 3]
  Basic Block 4:
L4:
  0: t6 = i + 1
  1: i = t6
  2: goto L0
  3: L2:
  前驱: [3], 后继: [1]
  Basic Block 5:
L5:
  0: i = 0
  1: j = 0
  2: L6:
  前驱: [3], 后继: []
  Basic Block 6:
L6:
  0: t7 = i < n
  1: if t7 goto L7 else goto L8
  2: j = 0
  3: L9:
  前驱: [8], 后继: [7, 8]
  Basic Block 7:
L7:
  0: t8 = j < n
  1: if t8 goto L10 else goto L11
  2: t9 = call getint()
  3: t10 = getelementptr B, [i, j]
  4: store t9 to t10
  5: t11 = j + 1
  6: j = t11
  7: goto L9
  8: L11:
  前驱: [6], 后继: [10, 11, 9]
  Basic Block 8:
L8:
  0: t12 = i + 1
  1: i = t12
  2: goto L6
  3: L8:
  前驱: [6], 后继: [6]
  Basic Block 9:
L9:
  0: t13 = call starttime()
  1: i = 0
  2: L12:
  前驱: [7], 后继: []
  Basic Block 10:
L10:
  0: t14 = i < 5
  1: if t14 goto L13 else goto L14
  2: t15 = call mm(n, A, B, C)
  3: t16 = call mm(n, A, C, B)
  4: t17 = i + 1
  5: i = t17
  6: goto L12
  7: L14:
  前驱: [7], 后继: [13, 14, 12]
  Basic Block 11:
L11:
  0: ans = 0
  1: i = 0
  2: L15:
  前驱: [7], 后继: []
  Basic Block 12:
L12:
  0: t18 = i < n
  1: if t18 goto L16 else goto L17
  2: j = 0
  3: L18:
  前驱: [10], 后继: [16, 17]
  Basic Block 13:
L13:
  前驱: [10], 后继: []
  Basic Block 14:
L14:
  0: t19 = j < n
  1: t21 = getelementptr B, [i, j]
  2: t20 = load t21
  3: t22 = ans + t20
  4: ans = t22
  5: t23 = j + 1
  6: j = t23
  7: L20:
  前驱: [10], 后继: []
  Basic Block 15:
L15:
  前驱: [12, 16], 后继: [16]
  Basic Block 16:
L16:
  0: t24 = i + 1
  1: i = t24
  2: goto L15
  3: L17:

  Basic Block 17:
L17:
  0: t25 = call stoptime()
  1: t26 = call putint(ans)
  2: t27 = call putch(10)
  3: return 0
  前驱: [12], 后继: []

Main Function ID: Some(1)
