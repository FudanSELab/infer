/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

struct Apple {
  int a;
  int b;
};

struct Person {
  struct Apple apple;
};

int main () {
  struct Person p;
  p.apple.a = 10;
  return 0;
}