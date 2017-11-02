Webgl Demo
================
Hao Ye
02 November, 2017

``` r
library(knitr)
library(rgl)
```

``` r
knit_hooks$set(webgl = hook_webgl)
```

``` r
x <- sort(rnorm(1000))
y <- rnorm(1000)
z <- rnorm(1000) + atan2(x, y)

plot3d(x, y, z, col = rainbow(1000))
```

<script>/*
* Copyright (C) 2009 Apple Inc. All Rights Reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions
* are met:
* 1. Redistributions of source code must retain the above copyright
*    notice, this list of conditions and the following disclaimer.
* 2. Redistributions in binary form must reproduce the above copyright
*    notice, this list of conditions and the following disclaimer in the
*    documentation and/or other materials provided with the distribution.
*
* THIS SOFTWARE IS PROVIDED BY APPLE INC. ``AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
* PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL APPLE INC. OR
* CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
* EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
* PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
* PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
* OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
* OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
* Copyright (2016) Duncan Murdoch - fixed CanvasMatrix4.ortho,
* cleaned up.
*/
/*
CanvasMatrix4 class
This class implements a 4x4 matrix. It has functions which
duplicate the functionality of the OpenGL matrix stack and
glut functions.
IDL:
[
Constructor(in CanvasMatrix4 matrix),           // copy passed matrix into new CanvasMatrix4
Constructor(in sequence<float> array)           // create new CanvasMatrix4 with 16 floats (row major)
Constructor()                                   // create new CanvasMatrix4 with identity matrix
]
interface CanvasMatrix4 {
attribute float m11;
attribute float m12;
attribute float m13;
attribute float m14;
attribute float m21;
attribute float m22;
attribute float m23;
attribute float m24;
attribute float m31;
attribute float m32;
attribute float m33;
attribute float m34;
attribute float m41;
attribute float m42;
attribute float m43;
attribute float m44;
void load(in CanvasMatrix4 matrix);                 // copy the values from the passed matrix
void load(in sequence<float> array);                // copy 16 floats into the matrix
sequence<float> getAsArray();                       // return the matrix as an array of 16 floats
WebGLFloatArray getAsCanvasFloatArray();           // return the matrix as a WebGLFloatArray with 16 values
void makeIdentity();                                // replace the matrix with identity
void transpose();                                   // replace the matrix with its transpose
void invert();                                      // replace the matrix with its inverse
void translate(in float x, in float y, in float z); // multiply the matrix by passed translation values on the right
void scale(in float x, in float y, in float z);     // multiply the matrix by passed scale values on the right
void rotate(in float angle,                         // multiply the matrix by passed rotation values on the right
in float x, in float y, in float z);    // (angle is in degrees)
void multRight(in CanvasMatrix matrix);             // multiply the matrix by the passed matrix on the right
void multLeft(in CanvasMatrix matrix);              // multiply the matrix by the passed matrix on the left
void ortho(in float left, in float right,           // multiply the matrix by the passed ortho values on the right
in float bottom, in float top,
in float near, in float far);
void frustum(in float left, in float right,         // multiply the matrix by the passed frustum values on the right
in float bottom, in float top,
in float near, in float far);
void perspective(in float fovy, in float aspect,    // multiply the matrix by the passed perspective values on the right
in float zNear, in float zFar);
void lookat(in float eyex, in float eyey, in float eyez,    // multiply the matrix by the passed lookat
in float ctrx, in float ctry, in float ctrz,    // values on the right
in float upx, in float upy, in float upz);
}
*/
CanvasMatrix4 = function(m)
{
if (typeof m == 'object') {
if ("length" in m && m.length >= 16) {
this.load(m[0], m[1], m[2], m[3], m[4], m[5], m[6], m[7], m[8], m[9], m[10], m[11], m[12], m[13], m[14], m[15]);
return;
}
else if (m instanceof CanvasMatrix4) {
this.load(m);
return;
}
}
this.makeIdentity();
};
CanvasMatrix4.prototype.load = function()
{
if (arguments.length == 1 && typeof arguments[0] == 'object') {
var matrix = arguments[0];
if ("length" in matrix && matrix.length == 16) {
this.m11 = matrix[0];
this.m12 = matrix[1];
this.m13 = matrix[2];
this.m14 = matrix[3];
this.m21 = matrix[4];
this.m22 = matrix[5];
this.m23 = matrix[6];
this.m24 = matrix[7];
this.m31 = matrix[8];
this.m32 = matrix[9];
this.m33 = matrix[10];
this.m34 = matrix[11];
this.m41 = matrix[12];
this.m42 = matrix[13];
this.m43 = matrix[14];
this.m44 = matrix[15];
return;
}
if (arguments[0] instanceof CanvasMatrix4) {
this.m11 = matrix.m11;
this.m12 = matrix.m12;
this.m13 = matrix.m13;
this.m14 = matrix.m14;
this.m21 = matrix.m21;
this.m22 = matrix.m22;
this.m23 = matrix.m23;
this.m24 = matrix.m24;
this.m31 = matrix.m31;
this.m32 = matrix.m32;
this.m33 = matrix.m33;
this.m34 = matrix.m34;
this.m41 = matrix.m41;
this.m42 = matrix.m42;
this.m43 = matrix.m43;
this.m44 = matrix.m44;
return;
}
}
this.makeIdentity();
};
CanvasMatrix4.prototype.getAsArray = function()
{
return [
this.m11, this.m12, this.m13, this.m14,
this.m21, this.m22, this.m23, this.m24,
this.m31, this.m32, this.m33, this.m34,
this.m41, this.m42, this.m43, this.m44
];
};
CanvasMatrix4.prototype.getAsWebGLFloatArray = function()
{
return new WebGLFloatArray(this.getAsArray());
};
CanvasMatrix4.prototype.makeIdentity = function()
{
this.m11 = 1;
this.m12 = 0;
this.m13 = 0;
this.m14 = 0;
this.m21 = 0;
this.m22 = 1;
this.m23 = 0;
this.m24 = 0;
this.m31 = 0;
this.m32 = 0;
this.m33 = 1;
this.m34 = 0;
this.m41 = 0;
this.m42 = 0;
this.m43 = 0;
this.m44 = 1;
};
CanvasMatrix4.prototype.transpose = function()
{
var tmp = this.m12;
this.m12 = this.m21;
this.m21 = tmp;
tmp = this.m13;
this.m13 = this.m31;
this.m31 = tmp;
tmp = this.m14;
this.m14 = this.m41;
this.m41 = tmp;
tmp = this.m23;
this.m23 = this.m32;
this.m32 = tmp;
tmp = this.m24;
this.m24 = this.m42;
this.m42 = tmp;
tmp = this.m34;
this.m34 = this.m43;
this.m43 = tmp;
};
CanvasMatrix4.prototype.invert = function()
{
// Calculate the 4x4 determinant
// If the determinant is zero,
// then the inverse matrix is not unique.
var det = this._determinant4x4();
if (Math.abs(det) < 1e-8)
return null;
this._makeAdjoint();
// Scale the adjoint matrix to get the inverse
this.m11 /= det;
this.m12 /= det;
this.m13 /= det;
this.m14 /= det;
this.m21 /= det;
this.m22 /= det;
this.m23 /= det;
this.m24 /= det;
this.m31 /= det;
this.m32 /= det;
this.m33 /= det;
this.m34 /= det;
this.m41 /= det;
this.m42 /= det;
this.m43 /= det;
this.m44 /= det;
};
CanvasMatrix4.prototype.translate = function(x,y,z)
{
if (x === undefined)
x = 0;
if (y === undefined)
y = 0;
if (z === undefined)
z = 0;
var matrix = new CanvasMatrix4();
matrix.m41 = x;
matrix.m42 = y;
matrix.m43 = z;
this.multRight(matrix);
};
CanvasMatrix4.prototype.scale = function(x,y,z)
{
if (x === undefined)
x = 1;
if (z === undefined) {
if (y === undefined) {
y = x;
z = x;
}
else
z = 1;
}
else if (y === undefined)
y = x;
var matrix = new CanvasMatrix4();
matrix.m11 = x;
matrix.m22 = y;
matrix.m33 = z;
this.multRight(matrix);
};
CanvasMatrix4.prototype.rotate = function(angle,x,y,z)
{
// angles are in degrees. Switch to radians
angle = angle / 180 * Math.PI;
angle /= 2;
var sinA = Math.sin(angle);
var cosA = Math.cos(angle);
var sinA2 = sinA * sinA;
// normalize
var length = Math.sqrt(x * x + y * y + z * z);
if (length === 0) {
// bad vector, just use something reasonable
x = 0;
y = 0;
z = 1;
} else if (length != 1) {
x /= length;
y /= length;
z /= length;
}
var mat = new CanvasMatrix4();
// optimize case where axis is along major axis
if (x == 1 && y === 0 && z === 0) {
mat.m11 = 1;
mat.m12 = 0;
mat.m13 = 0;
mat.m21 = 0;
mat.m22 = 1 - 2 * sinA2;
mat.m23 = 2 * sinA * cosA;
mat.m31 = 0;
mat.m32 = -2 * sinA * cosA;
mat.m33 = 1 - 2 * sinA2;
mat.m14 = mat.m24 = mat.m34 = 0;
mat.m41 = mat.m42 = mat.m43 = 0;
mat.m44 = 1;
} else if (x === 0 && y == 1 && z === 0) {
mat.m11 = 1 - 2 * sinA2;
mat.m12 = 0;
mat.m13 = -2 * sinA * cosA;
mat.m21 = 0;
mat.m22 = 1;
mat.m23 = 0;
mat.m31 = 2 * sinA * cosA;
mat.m32 = 0;
mat.m33 = 1 - 2 * sinA2;
mat.m14 = mat.m24 = mat.m34 = 0;
mat.m41 = mat.m42 = mat.m43 = 0;
mat.m44 = 1;
} else if (x === 0 && y === 0 && z == 1) {
mat.m11 = 1 - 2 * sinA2;
mat.m12 = 2 * sinA * cosA;
mat.m13 = 0;
mat.m21 = -2 * sinA * cosA;
mat.m22 = 1 - 2 * sinA2;
mat.m23 = 0;
mat.m31 = 0;
mat.m32 = 0;
mat.m33 = 1;
mat.m14 = mat.m24 = mat.m34 = 0;
mat.m41 = mat.m42 = mat.m43 = 0;
mat.m44 = 1;
} else {
var x2 = x*x;
var y2 = y*y;
var z2 = z*z;
mat.m11 = 1 - 2 * (y2 + z2) * sinA2;
mat.m12 = 2 * (x * y * sinA2 + z * sinA * cosA);
mat.m13 = 2 * (x * z * sinA2 - y * sinA * cosA);
mat.m21 = 2 * (y * x * sinA2 - z * sinA * cosA);
mat.m22 = 1 - 2 * (z2 + x2) * sinA2;
mat.m23 = 2 * (y * z * sinA2 + x * sinA * cosA);
mat.m31 = 2 * (z * x * sinA2 + y * sinA * cosA);
mat.m32 = 2 * (z * y * sinA2 - x * sinA * cosA);
mat.m33 = 1 - 2 * (x2 + y2) * sinA2;
mat.m14 = mat.m24 = mat.m34 = 0;
mat.m41 = mat.m42 = mat.m43 = 0;
mat.m44 = 1;
}
this.multRight(mat);
};
CanvasMatrix4.prototype.multRight = function(mat)
{
var m11 = (this.m11 * mat.m11 + this.m12 * mat.m21 +
this.m13 * mat.m31 + this.m14 * mat.m41);
var m12 = (this.m11 * mat.m12 + this.m12 * mat.m22 +
this.m13 * mat.m32 + this.m14 * mat.m42);
var m13 = (this.m11 * mat.m13 + this.m12 * mat.m23 +
this.m13 * mat.m33 + this.m14 * mat.m43);
var m14 = (this.m11 * mat.m14 + this.m12 * mat.m24 +
this.m13 * mat.m34 + this.m14 * mat.m44);
var m21 = (this.m21 * mat.m11 + this.m22 * mat.m21 +
this.m23 * mat.m31 + this.m24 * mat.m41);
var m22 = (this.m21 * mat.m12 + this.m22 * mat.m22 +
this.m23 * mat.m32 + this.m24 * mat.m42);
var m23 = (this.m21 * mat.m13 + this.m22 * mat.m23 +
this.m23 * mat.m33 + this.m24 * mat.m43);
var m24 = (this.m21 * mat.m14 + this.m22 * mat.m24 +
this.m23 * mat.m34 + this.m24 * mat.m44);
var m31 = (this.m31 * mat.m11 + this.m32 * mat.m21 +
this.m33 * mat.m31 + this.m34 * mat.m41);
var m32 = (this.m31 * mat.m12 + this.m32 * mat.m22 +
this.m33 * mat.m32 + this.m34 * mat.m42);
var m33 = (this.m31 * mat.m13 + this.m32 * mat.m23 +
this.m33 * mat.m33 + this.m34 * mat.m43);
var m34 = (this.m31 * mat.m14 + this.m32 * mat.m24 +
this.m33 * mat.m34 + this.m34 * mat.m44);
var m41 = (this.m41 * mat.m11 + this.m42 * mat.m21 +
this.m43 * mat.m31 + this.m44 * mat.m41);
var m42 = (this.m41 * mat.m12 + this.m42 * mat.m22 +
this.m43 * mat.m32 + this.m44 * mat.m42);
var m43 = (this.m41 * mat.m13 + this.m42 * mat.m23 +
this.m43 * mat.m33 + this.m44 * mat.m43);
var m44 = (this.m41 * mat.m14 + this.m42 * mat.m24 +
this.m43 * mat.m34 + this.m44 * mat.m44);
this.m11 = m11;
this.m12 = m12;
this.m13 = m13;
this.m14 = m14;
this.m21 = m21;
this.m22 = m22;
this.m23 = m23;
this.m24 = m24;
this.m31 = m31;
this.m32 = m32;
this.m33 = m33;
this.m34 = m34;
this.m41 = m41;
this.m42 = m42;
this.m43 = m43;
this.m44 = m44;
};
CanvasMatrix4.prototype.multLeft = function(mat)
{
var m11 = (mat.m11 * this.m11 + mat.m12 * this.m21 +
mat.m13 * this.m31 + mat.m14 * this.m41);
var m12 = (mat.m11 * this.m12 + mat.m12 * this.m22 +
mat.m13 * this.m32 + mat.m14 * this.m42);
var m13 = (mat.m11 * this.m13 + mat.m12 * this.m23 +
mat.m13 * this.m33 + mat.m14 * this.m43);
var m14 = (mat.m11 * this.m14 + mat.m12 * this.m24 +
mat.m13 * this.m34 + mat.m14 * this.m44);
var m21 = (mat.m21 * this.m11 + mat.m22 * this.m21 +
mat.m23 * this.m31 + mat.m24 * this.m41);
var m22 = (mat.m21 * this.m12 + mat.m22 * this.m22 +
mat.m23 * this.m32 + mat.m24 * this.m42);
var m23 = (mat.m21 * this.m13 + mat.m22 * this.m23 +
mat.m23 * this.m33 + mat.m24 * this.m43);
var m24 = (mat.m21 * this.m14 + mat.m22 * this.m24 +
mat.m23 * this.m34 + mat.m24 * this.m44);
var m31 = (mat.m31 * this.m11 + mat.m32 * this.m21 +
mat.m33 * this.m31 + mat.m34 * this.m41);
var m32 = (mat.m31 * this.m12 + mat.m32 * this.m22 +
mat.m33 * this.m32 + mat.m34 * this.m42);
var m33 = (mat.m31 * this.m13 + mat.m32 * this.m23 +
mat.m33 * this.m33 + mat.m34 * this.m43);
var m34 = (mat.m31 * this.m14 + mat.m32 * this.m24 +
mat.m33 * this.m34 + mat.m34 * this.m44);
var m41 = (mat.m41 * this.m11 + mat.m42 * this.m21 +
mat.m43 * this.m31 + mat.m44 * this.m41);
var m42 = (mat.m41 * this.m12 + mat.m42 * this.m22 +
mat.m43 * this.m32 + mat.m44 * this.m42);
var m43 = (mat.m41 * this.m13 + mat.m42 * this.m23 +
mat.m43 * this.m33 + mat.m44 * this.m43);
var m44 = (mat.m41 * this.m14 + mat.m42 * this.m24 +
mat.m43 * this.m34 + mat.m44 * this.m44);
this.m11 = m11;
this.m12 = m12;
this.m13 = m13;
this.m14 = m14;
this.m21 = m21;
this.m22 = m22;
this.m23 = m23;
this.m24 = m24;
this.m31 = m31;
this.m32 = m32;
this.m33 = m33;
this.m34 = m34;
this.m41 = m41;
this.m42 = m42;
this.m43 = m43;
this.m44 = m44;
};
CanvasMatrix4.prototype.ortho = function(left, right, bottom, top, near, far)
{
var tx = (left + right) / (left - right);
var ty = (top + bottom) / (bottom - top);
var tz = (far + near) / (near - far);
var matrix = new CanvasMatrix4();
matrix.m11 = 2 / (right - left);
matrix.m12 = 0;
matrix.m13 = 0;
matrix.m14 = 0;
matrix.m21 = 0;
matrix.m22 = 2 / (top - bottom);
matrix.m23 = 0;
matrix.m24 = 0;
matrix.m31 = 0;
matrix.m32 = 0;
matrix.m33 = -2 / (far - near);
matrix.m34 = 0;
matrix.m41 = tx;
matrix.m42 = ty;
matrix.m43 = tz;
matrix.m44 = 1;
this.multRight(matrix);
};
CanvasMatrix4.prototype.frustum = function(left, right, bottom, top, near, far)
{
var matrix = new CanvasMatrix4();
var A = (right + left) / (right - left);
var B = (top + bottom) / (top - bottom);
var C = -(far + near) / (far - near);
var D = -(2 * far * near) / (far - near);
matrix.m11 = (2 * near) / (right - left);
matrix.m12 = 0;
matrix.m13 = 0;
matrix.m14 = 0;
matrix.m21 = 0;
matrix.m22 = 2 * near / (top - bottom);
matrix.m23 = 0;
matrix.m24 = 0;
matrix.m31 = A;
matrix.m32 = B;
matrix.m33 = C;
matrix.m34 = -1;
matrix.m41 = 0;
matrix.m42 = 0;
matrix.m43 = D;
matrix.m44 = 0;
this.multRight(matrix);
};
CanvasMatrix4.prototype.perspective = function(fovy, aspect, zNear, zFar)
{
var top = Math.tan(fovy * Math.PI / 360) * zNear;
var bottom = -top;
var left = aspect * bottom;
var right = aspect * top;
this.frustum(left, right, bottom, top, zNear, zFar);
};
CanvasMatrix4.prototype.lookat = function(eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz)
{
var matrix = new CanvasMatrix4();
// Make rotation matrix
// Z vector
var zx = eyex - centerx;
var zy = eyey - centery;
var zz = eyez - centerz;
var mag = Math.sqrt(zx * zx + zy * zy + zz * zz);
if (mag) {
zx /= mag;
zy /= mag;
zz /= mag;
}
// Y vector
var yx = upx;
var yy = upy;
var yz = upz;
// X vector = Y cross Z
xx =  yy * zz - yz * zy;
xy = -yx * zz + yz * zx;
xz =  yx * zy - yy * zx;
// Recompute Y = Z cross X
yx = zy * xz - zz * xy;
yy = -zx * xz + zz * xx;
yx = zx * xy - zy * xx;
// cross product gives area of parallelogram, which is < 1.0 for
// non-perpendicular unit-length vectors; so normalize x, y here
mag = Math.sqrt(xx * xx + xy * xy + xz * xz);
if (mag) {
xx /= mag;
xy /= mag;
xz /= mag;
}
mag = Math.sqrt(yx * yx + yy * yy + yz * yz);
if (mag) {
yx /= mag;
yy /= mag;
yz /= mag;
}
matrix.m11 = xx;
matrix.m12 = xy;
matrix.m13 = xz;
matrix.m14 = 0;
matrix.m21 = yx;
matrix.m22 = yy;
matrix.m23 = yz;
matrix.m24 = 0;
matrix.m31 = zx;
matrix.m32 = zy;
matrix.m33 = zz;
matrix.m34 = 0;
matrix.m41 = 0;
matrix.m42 = 0;
matrix.m43 = 0;
matrix.m44 = 1;
matrix.translate(-eyex, -eyey, -eyez);
this.multRight(matrix);
};
// Support functions
CanvasMatrix4.prototype._determinant2x2 = function(a, b, c, d)
{
return a * d - b * c;
};
CanvasMatrix4.prototype._determinant3x3 = function(a1, a2, a3, b1, b2, b3, c1, c2, c3)
{
return a1 * this._determinant2x2(b2, b3, c2, c3) -
b1 * this._determinant2x2(a2, a3, c2, c3) +
c1 * this._determinant2x2(a2, a3, b2, b3);
};
CanvasMatrix4.prototype._determinant4x4 = function()
{
var a1 = this.m11;
var b1 = this.m12;
var c1 = this.m13;
var d1 = this.m14;
var a2 = this.m21;
var b2 = this.m22;
var c2 = this.m23;
var d2 = this.m24;
var a3 = this.m31;
var b3 = this.m32;
var c3 = this.m33;
var d3 = this.m34;
var a4 = this.m41;
var b4 = this.m42;
var c4 = this.m43;
var d4 = this.m44;
return a1 * this._determinant3x3(b2, b3, b4, c2, c3, c4, d2, d3, d4) -
b1 * this._determinant3x3(a2, a3, a4, c2, c3, c4, d2, d3, d4) +
c1 * this._determinant3x3(a2, a3, a4, b2, b3, b4, d2, d3, d4) -
d1 * this._determinant3x3(a2, a3, a4, b2, b3, b4, c2, c3, c4);
};
CanvasMatrix4.prototype._makeAdjoint = function()
{
var a1 = this.m11;
var b1 = this.m12;
var c1 = this.m13;
var d1 = this.m14;
var a2 = this.m21;
var b2 = this.m22;
var c2 = this.m23;
var d2 = this.m24;
var a3 = this.m31;
var b3 = this.m32;
var c3 = this.m33;
var d3 = this.m34;
var a4 = this.m41;
var b4 = this.m42;
var c4 = this.m43;
var d4 = this.m44;
// Row column labeling reversed since we transpose rows & columns
this.m11  =   this._determinant3x3(b2, b3, b4, c2, c3, c4, d2, d3, d4);
this.m21  = - this._determinant3x3(a2, a3, a4, c2, c3, c4, d2, d3, d4);
this.m31  =   this._determinant3x3(a2, a3, a4, b2, b3, b4, d2, d3, d4);
this.m41  = - this._determinant3x3(a2, a3, a4, b2, b3, b4, c2, c3, c4);
this.m12  = - this._determinant3x3(b1, b3, b4, c1, c3, c4, d1, d3, d4);
this.m22  =   this._determinant3x3(a1, a3, a4, c1, c3, c4, d1, d3, d4);
this.m32  = - this._determinant3x3(a1, a3, a4, b1, b3, b4, d1, d3, d4);
this.m42  =   this._determinant3x3(a1, a3, a4, b1, b3, b4, c1, c3, c4);
this.m13  =   this._determinant3x3(b1, b2, b4, c1, c2, c4, d1, d2, d4);
this.m23  = - this._determinant3x3(a1, a2, a4, c1, c2, c4, d1, d2, d4);
this.m33  =   this._determinant3x3(a1, a2, a4, b1, b2, b4, d1, d2, d4);
this.m43  = - this._determinant3x3(a1, a2, a4, b1, b2, b4, c1, c2, c4);
this.m14  = - this._determinant3x3(b1, b2, b3, c1, c2, c3, d1, d2, d3);
this.m24  =   this._determinant3x3(a1, a2, a3, c1, c2, c3, d1, d2, d3);
this.m34  = - this._determinant3x3(a1, a2, a3, b1, b2, b3, d1, d2, d3);
this.m44  =   this._determinant3x3(a1, a2, a3, b1, b2, b3, c1, c2, c3);
};</script>
<script>
rglwidgetClass = function() {
this.canvas = null;
this.userMatrix = new CanvasMatrix4();
this.types = [];
this.prMatrix = new CanvasMatrix4();
this.mvMatrix = new CanvasMatrix4();
this.vp = null;
this.prmvMatrix = null;
this.origs = null;
this.gl = null;
this.scene = null;
};
(function() {
this.multMV = function(M, v) {
return [ M.m11 * v[0] + M.m12 * v[1] + M.m13 * v[2] + M.m14 * v[3],
M.m21 * v[0] + M.m22 * v[1] + M.m23 * v[2] + M.m24 * v[3],
M.m31 * v[0] + M.m32 * v[1] + M.m33 * v[2] + M.m34 * v[3],
M.m41 * v[0] + M.m42 * v[1] + M.m43 * v[2] + M.m44 * v[3]
];
};
this.vlen = function(v) {
return Math.sqrt(this.dotprod(v, v));
};
this.dotprod = function(a, b) {
return a[0]*b[0] + a[1]*b[1] + a[2]*b[2];
};
this.xprod = function(a, b) {
return [a[1]*b[2] - a[2]*b[1],
a[2]*b[0] - a[0]*b[2],
a[0]*b[1] - a[1]*b[0]];
};
this.cbind = function(a, b) {
if (b.length < a.length)
b = this.repeatToLen(b, a.length);
else if (a.length < b.length)
a = this.repeatToLen(a, b.length);
return a.map(function(currentValue, index, array) {
return currentValue.concat(b[index]);
});
};
this.swap = function(a, i, j) {
var temp = a[i];
a[i] = a[j];
a[j] = temp;
};
this.flatten = function(a) {
return [].concat.apply([], a);
};
/* set element of 1d or 2d array as if it was flattened.  Column major, zero based! */
this.setElement = function(a, i, value) {
if (Array.isArray(a[0])) {
var dim = a.length,
col = Math.floor(i/dim),
row = i % dim;
a[row][col] = value;
} else {
a[i] = value;
}
};
this.transpose = function(a) {
var newArray = [],
n = a.length,
m = a[0].length,
i;
for(i = 0; i < m; i++){
newArray.push([]);
}
for(i = 0; i < n; i++){
for(var j = 0; j < m; j++){
newArray[j].push(a[i][j]);
}
}
return newArray;
};
this.sumsq = function(x) {
var result = 0, i;
for (i=0; i < x.length; i++)
result += x[i]*x[i];
return result;
};
this.toCanvasMatrix4 = function(mat) {
if (mat instanceof CanvasMatrix4)
return mat;
var result = new CanvasMatrix4();
mat = this.flatten(this.transpose(mat));
result.load(mat);
return result;
};
this.stringToRgb = function(s) {
s = s.replace("#", "");
var bigint = parseInt(s, 16);
return [((bigint >> 16) & 255)/255,
((bigint >> 8) & 255)/255,
(bigint & 255)/255];
};
this.componentProduct = function(x, y) {
if (typeof y === "undefined") {
this.alertOnce("Bad arg to componentProduct");
}
var result = new Float32Array(3), i;
for (i = 0; i<3; i++)
result[i] = x[i]*y[i];
return result;
};
this.getPowerOfTwo = function(value) {
var pow = 1;
while(pow<value) {
pow *= 2;
}
return pow;
};
this.unique = function(arr) {
arr = [].concat(arr);
return arr.filter(function(value, index, self) {
return self.indexOf(value) === index;
});
};
this.repeatToLen = function(arr, len) {
arr = [].concat(arr);
while (arr.length < len/2)
arr = arr.concat(arr);
return arr.concat(arr.slice(0, len - arr.length));
};
this.alertOnce = function(msg) {
if (typeof this.alerted !== "undefined")
return;
this.alerted = true;
alert(msg);
};
this.f_is_lit = 1;
this.f_is_smooth = 2;
this.f_has_texture = 4;
this.f_is_indexed = 8;
this.f_depth_sort = 16;
this.f_fixed_quads = 32;
this.f_is_transparent = 64;
this.f_is_lines = 128;
this.f_sprites_3d = 256;
this.f_sprite_3d = 512;
this.f_is_subscene = 1024;
this.f_is_clipplanes = 2048;
this.f_fixed_size = 4096;
this.f_is_points = 8192;
this.f_is_twosided = 16384;
this.whichList = function(id) {
var obj = this.getObj(id),
flags = obj.flags;
if (obj.type === "light")
return "lights";
if (flags & this.f_is_subscene)
return "subscenes";
if (flags & this.f_is_clipplanes)
return "clipplanes";
if (flags & this.f_is_transparent)
return "transparent";
return "opaque";
};
this.getObj = function(id) {
if (typeof id !== "number") {
this.alertOnce("getObj id is "+typeof id);
}
return this.scene.objects[id];
};
this.getIdsByType = function(type, subscene) {
var
result = [], i, self = this;
if (typeof subscene === "undefined") {
Object.keys(this.scene.objects).forEach(
function(key) {
key = parseInt(key, 10);
if (self.getObj(key).type === type)
result.push(key);
});
} else {
ids = this.getObj(subscene).objects;
for (i=0; i < ids.length; i++) {
if (this.getObj(ids[i]).type === type) {
result.push(ids[i]);
}
}
}
return result;
};
this.getMaterial = function(id, property) {
var obj = this.getObj(id),
mat = obj.material[property];
if (typeof mat === "undefined")
mat = this.scene.material[property];
return mat;
};
this.inSubscene = function(id, subscene) {
return this.getObj(subscene).objects.indexOf(id) > -1;
};
this.addToSubscene = function(id, subscene) {
var thelist,
thesub = this.getObj(subscene),
ids = [id],
obj = this.getObj(id), i;
if (typeof obj.newIds !== "undefined") {
ids = ids.concat(obj.newIds);
}
for (i = 0; i < ids.length; i++) {
id = ids[i];
if (thesub.objects.indexOf(id) == -1) {
thelist = this.whichList(id);
thesub.objects.push(id);
thesub[thelist].push(id);
}
}
};
this.delFromSubscene = function(id, subscene) {
var thelist,
thesub = this.getObj(subscene),
obj = this.getObj(id),
ids = [id], i;
if (typeof obj.newIds !== "undefined")
ids = ids.concat(obj.newIds);
for (j=0; j<ids.length;j++) {
id = ids[j];
i = thesub.objects.indexOf(id);
if (i > -1) {
thesub.objects.splice(i, 1);
thelist = this.whichList(id);
i = thesub[thelist].indexOf(id);
thesub[thelist].splice(i, 1);
}
}
};
this.setSubsceneEntries = function(ids, subsceneid) {
var sub = this.getObj(subsceneid);
sub.objects = ids;
this.initSubscene(subsceneid);
};
this.getSubsceneEntries = function(subscene) {
return this.getObj(subscene).objects;
};
this.getChildSubscenes = function(subscene) {
return this.getObj(subscene).subscenes;
};
this.startDrawing = function() {
var value = this.drawing;
this.drawing = true;
return value;
};
this.stopDrawing = function(saved) {
this.drawing = saved;
if (!saved && this.gl && this.gl.isContextLost())
this.restartCanvas();
};
this.getVertexShader = function(id) {
var obj = this.getObj(id),
userShader = obj.userVertexShader,
flags = obj.flags,
type = obj.type,
is_lit = flags & this.f_is_lit,
has_texture = flags & this.f_has_texture,
fixed_quads = flags & this.f_fixed_quads,
sprites_3d = flags & this.f_sprites_3d,
sprite_3d = flags & this.f_sprite_3d,
nclipplanes = this.countClipplanes(),
fixed_size = flags & this.f_fixed_size,
is_points = flags & this.f_is_points,
is_twosided = flags & this.f_is_twosided,
result;
if (type === "clipplanes" || sprites_3d) return;
if (typeof userShader !== "undefined") return userShader;
result = "  /* ****** "+type+" object "+id+" vertex shader ****** */\n"+
"  attribute vec3 aPos;\n"+
"  attribute vec4 aCol;\n"+
" uniform mat4 mvMatrix;\n"+
" uniform mat4 prMatrix;\n"+
" varying vec4 vCol;\n"+
" varying vec4 vPosition;\n";
if ((is_lit && !fixed_quads) || sprite_3d)
result = result + "  attribute vec3 aNorm;\n"+
" uniform mat4 normMatrix;\n"+
" varying vec3 vNormal;\n";
if (has_texture || type === "text")
result = result + " attribute vec2 aTexcoord;\n"+
" varying vec2 vTexcoord;\n";
if (fixed_size)
result = result + "  uniform vec2 textScale;\n";
if (fixed_quads)
result = result + "  attribute vec2 aOfs;\n";
else if (sprite_3d)
result = result + "  uniform vec3 uOrig;\n"+
"  uniform float uSize;\n"+
"  uniform mat4 usermat;\n";
if (is_twosided)
result = result + "  attribute vec3 aPos1;\n"+
"  attribute vec3 aPos2;\n"+
"  varying float normz;\n";
result = result + "  void main(void) {\n";
if (nclipplanes || (!fixed_quads && !sprite_3d))
result = result + "    vPosition = mvMatrix * vec4(aPos, 1.);\n";
if (!fixed_quads && !sprite_3d)
result = result + "    gl_Position = prMatrix * vPosition;\n";
if (is_points) {
var size = this.getMaterial(id, "size");
result = result + "    gl_PointSize = "+size.toFixed(1)+";\n";
}
result = result + "    vCol = aCol;\n";
if (is_lit && !fixed_quads && !sprite_3d)
result = result + "    vNormal = normalize((normMatrix * vec4(aNorm, 1.)).xyz);\n";
if (has_texture || type == "text")
result = result + "    vTexcoord = aTexcoord;\n";
if (fixed_size)
result = result + "    vec4 pos = prMatrix * mvMatrix * vec4(aPos, 1.);\n"+
"   pos = pos/pos.w;\n"+
"   gl_Position = pos + vec4(aOfs*textScale, 0.,0.);\n";
if (type == "sprites" && !fixed_size)
result = result + "    vec4 pos = mvMatrix * vec4(aPos, 1.);\n"+
"   pos = pos/pos.w + vec4(aOfs, 0., 0.);\n"+
"   gl_Position = prMatrix*pos;\n";
if (sprite_3d)
result = result + "   vNormal = normalize((normMatrix * vec4(aNorm, 1.)).xyz);\n"+
"   vec4 pos = mvMatrix * vec4(uOrig, 1.);\n"+
"   vPosition = pos/pos.w + vec4(uSize*(vec4(aPos, 1.)*usermat).xyz,0.);\n"+
"   gl_Position = prMatrix * vPosition;\n";
if (is_twosided)
result = result + "   vec4 pos1 = prMatrix*(mvMatrix*vec4(aPos1, 1.));\n"+
"   pos1 = pos1/pos1.w - gl_Position/gl_Position.w;\n"+
"   vec4 pos2 = prMatrix*(mvMatrix*vec4(aPos2, 1.));\n"+
"   pos2 = pos2/pos2.w - gl_Position/gl_Position.w;\n"+
"   normz = pos1.x*pos2.y - pos1.y*pos2.x;\n";
result = result + "  }\n";
// console.log(result);
return result;
};
this.getFragmentShader = function(id) {
var obj = this.getObj(id),
userShader = obj.userFragmentShader,
flags = obj.flags,
type = obj.type,
is_lit = flags & this.f_is_lit,
has_texture = flags & this.f_has_texture,
fixed_quads = flags & this.f_fixed_quads,
sprites_3d = flags & this.f_sprites_3d,
is_twosided = (flags & this.f_is_twosided) > 0,
nclipplanes = this.countClipplanes(), i,
texture_format, nlights,
result;
if (type === "clipplanes" || sprites_3d) return;
if (typeof userShader !== "undefined") return userShader;
if (has_texture)
texture_format = this.getMaterial(id, "textype");
result = "/* ****** "+type+" object "+id+" fragment shader ****** */\n"+
"#ifdef GL_ES\n"+
"  precision highp float;\n"+
"#endif\n"+
"  varying vec4 vCol; // carries alpha\n"+
"  varying vec4 vPosition;\n";
if (has_texture || type === "text")
result = result + "  varying vec2 vTexcoord;\n"+
" uniform sampler2D uSampler;\n";
if (is_lit && !fixed_quads)
result = result + "  varying vec3 vNormal;\n";
for (i = 0; i < nclipplanes; i++)
result = result + "  uniform vec4 vClipplane"+i+";\n";
if (is_lit) {
nlights = this.countLights();
if (nlights)
result = result + "  uniform mat4 mvMatrix;\n";
else
is_lit = false;
}
if (is_lit) {
result = result + "   uniform vec3 emission;\n"+
"   uniform float shininess;\n";
for (i=0; i < nlights; i++) {
result = result + "   uniform vec3 ambient" + i + ";\n"+
"   uniform vec3 specular" + i +"; // light*material\n"+
"   uniform vec3 diffuse" + i + ";\n"+
"   uniform vec3 lightDir" + i + ";\n"+
"   uniform bool viewpoint" + i + ";\n"+
"   uniform bool finite" + i + ";\n";
}
}
if (is_twosided) 
result = result + "   uniform bool front;\n"+
"   varying float normz;\n";
result = result + "  void main(void) {\n";
for (i=0; i < nclipplanes;i++)
result = result + "    if (dot(vPosition, vClipplane"+i+") < 0.0) discard;\n";
if (fixed_quads) {
result = result +   "    vec3 n = vec3(0., 0., 1.);\n"; 
} else if (is_lit) {
result = result +   "    vec3 n = normalize(vNormal);\n";
}
if (is_twosided) {
result = result +   "    if ((normz <= 0.) != front) discard;";
}
if (is_lit) {
result = result + "    vec3 eye = normalize(-vPosition.xyz);\n"+
"   vec3 lightdir;\n"+
"   vec4 colDiff;\n"+
"   vec3 halfVec;\n"+
"   vec4 lighteffect = vec4(emission, 0.);\n"+
"   vec3 col;\n"+
"   float nDotL;\n";
if (!fixed_quads) {
result = result +   "   n = -faceforward(n, n, eye);\n";
}
for (i=0; i < nlights; i++) {
result = result + "   colDiff = vec4(vCol.rgb * diffuse" + i + ", vCol.a);\n"+
"   lightdir = lightDir" + i + ";\n"+
"   if (!viewpoint" + i +")\n"+
"     lightdir = (mvMatrix * vec4(lightdir, 1.)).xyz;\n"+
"   if (!finite" + i + ") {\n"+
"     halfVec = normalize(lightdir + eye);\n"+
"   } else {\n"+
"     lightdir = normalize(lightdir - vPosition.xyz);\n"+
"     halfVec = normalize(lightdir + eye);\n"+
"   }\n"+
"    col = ambient" + i + ";\n"+
"   nDotL = dot(n, lightdir);\n"+
"   col = col + max(nDotL, 0.) * colDiff.rgb;\n"+
"   col = col + pow(max(dot(halfVec, n), 0.), shininess) * specular" + i + ";\n"+
"   lighteffect = lighteffect + vec4(col, colDiff.a);\n";
}
} else {
result = result +   "   vec4 colDiff = vCol;\n"+
"    vec4 lighteffect = colDiff;\n";
}
if (type === "text")
result = result +   "    vec4 textureColor = lighteffect*texture2D(uSampler, vTexcoord);\n";
if (has_texture) {
result = result + {
rgb:            "   vec4 textureColor = lighteffect*vec4(texture2D(uSampler, vTexcoord).rgb, 1.);\n",
rgba:           "   vec4 textureColor = lighteffect*texture2D(uSampler, vTexcoord);\n",
alpha:          "   vec4 textureColor = texture2D(uSampler, vTexcoord);\n"+
"   float luminance = dot(vec3(1.,1.,1.), textureColor.rgb)/3.;\n"+
"   textureColor =  vec4(lighteffect.rgb, lighteffect.a*luminance);\n",
luminance:      "   vec4 textureColor = vec4(lighteffect.rgb*dot(texture2D(uSampler, vTexcoord).rgb, vec3(1.,1.,1.))/3., lighteffect.a);\n",
"luminance.alpha":"    vec4 textureColor = texture2D(uSampler, vTexcoord);\n"+
"   float luminance = dot(vec3(1.,1.,1.),textureColor.rgb)/3.;\n"+
"   textureColor = vec4(lighteffect.rgb*luminance, lighteffect.a*textureColor.a);\n"
}[texture_format]+
"   gl_FragColor = textureColor;\n";
} else if (type === "text") {
result = result +   "    if (textureColor.a < 0.1)\n"+
"     discard;\n"+
"   else\n"+
"     gl_FragColor = textureColor;\n";
} else
result = result +   "   gl_FragColor = lighteffect;\n";
result = result + "  }\n";
// console.log(result);
return result;
};
this.getShader = function(shaderType, code) {
var gl = this.gl, shader;
shader = gl.createShader(shaderType);
gl.shaderSource(shader, code);
gl.compileShader(shader);
if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS) && !gl.isContextLost())
alert(gl.getShaderInfoLog(shader));
return shader;
};
this.handleLoadedTexture = function(texture, textureCanvas) { 
var gl = this.gl || this.initGL();
gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
gl.bindTexture(gl.TEXTURE_2D, texture);
gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, textureCanvas);
gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_NEAREST);
gl.generateMipmap(gl.TEXTURE_2D);
gl.bindTexture(gl.TEXTURE_2D, null);
};
this.loadImageToTexture = function(uri, texture) {
var canvas = this.textureCanvas,
ctx = canvas.getContext("2d"),
image = new Image(),
self = this;
image.onload = function() {
var w = image.width,
h = image.height,
canvasX = self.getPowerOfTwo(w),
canvasY = self.getPowerOfTwo(h),
gl = self.gl || self.initGL(),
maxTexSize = gl.getParameter(gl.MAX_TEXTURE_SIZE);
if (maxTexSize > 4096) maxTexSize = 4096;
while (canvasX > 1 && canvasY > 1 && (canvasX > maxTexSize || canvasY > maxTexSize)) {
canvasX /= 2;
canvasY /= 2;
}
canvas.width = canvasX;
canvas.height = canvasY;
ctx.imageSmoothingEnabled = true;
ctx.drawImage(image, 0, 0, canvasX, canvasY);
self.handleLoadedTexture(texture, canvas);
self.drawScene();
};
image.src = uri;
};
this.drawTextToCanvas = function(text, cex, family, font) {
var canvasX, canvasY,
textY,
scaling = 20,
textColour = "white",
backgroundColour = "rgba(0,0,0,0)",
canvas = this.textureCanvas,
ctx = canvas.getContext("2d"),
i, textHeights = [], widths = [], offset = 0, offsets = [],
fontStrings = [],
getFontString = function(i) {
textHeights[i] = scaling*cex[i];
var fontString = textHeights[i] + "px",
family0 = family[i],
font0 = font[i];
if (family0 === "sans")
family0 = "sans-serif";
else if (family0 === "mono")
family0 = "monospace";
fontString = fontString + " " + family0;
if (font0 === 2 || font0 === 4)
fontString = "bold " + fontString;
if (font0 === 3 || font0 === 4)
fontString = "italic " + fontString;
return fontString;
};
cex = this.repeatToLen(cex, text.length);
family = this.repeatToLen(family, text.length);
font = this.repeatToLen(font, text.length);
canvasX = 1;
for (i = 0; i < text.length; i++)  {
ctx.font = fontStrings[i] = getFontString(i);
widths[i] = ctx.measureText(text[i]).width;
offset = offsets[i] = offset + 2*textHeights[i];
canvasX = (widths[i] > canvasX) ? widths[i] : canvasX;
}
canvasX = this.getPowerOfTwo(canvasX);
canvasY = this.getPowerOfTwo(offset);
canvas.width = canvasX;
canvas.height = canvasY;
ctx.fillStyle = backgroundColour;
ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);
ctx.textBaseline = "alphabetic";
for(i = 0; i < text.length; i++) {
textY = offsets[i];
ctx.font = fontStrings[i];
ctx.fillStyle = textColour;
ctx.textAlign = "left";
ctx.fillText(text[i], 0,  textY);
}
return {canvasX:canvasX, canvasY:canvasY,
widths:widths, textHeights:textHeights,
offsets:offsets};
};
this.setViewport = function(id) {
var gl = this.gl || this.initGL(),
vp = this.getObj(id).par3d.viewport,
x = vp.x*this.canvas.width,
y = vp.y*this.canvas.height,
width = vp.width*this.canvas.width,
height = vp.height*this.canvas.height;
this.vp = {x:x, y:y, width:width, height:height};
gl.viewport(x, y, width, height);
gl.scissor(x, y, width, height);
gl.enable(gl.SCISSOR_TEST);
};
this.setprMatrix = function(id) {
var subscene = this.getObj(id),
embedding = subscene.embeddings.projection;
if (embedding === "replace")
this.prMatrix.makeIdentity();
else
this.setprMatrix(subscene.parent);
if (embedding === "inherit")
return;
// This is based on the Frustum::enclose code from geom.cpp
var bbox = subscene.par3d.bbox,
scale = subscene.par3d.scale,
ranges = [(bbox[1]-bbox[0])*scale[0]/2,
(bbox[3]-bbox[2])*scale[1]/2,
(bbox[5]-bbox[4])*scale[2]/2],
radius = Math.sqrt(this.sumsq(ranges))*1.1; // A bit bigger to handle labels
if (radius <= 0) radius = 1;
var observer = subscene.par3d.observer,
distance = observer[2],
FOV = subscene.par3d.FOV, ortho = FOV === 0,
t = ortho ? 1 : Math.tan(FOV*Math.PI/360),
near = distance - radius,
far = distance + radius,
hlen,
aspect = this.vp.width/this.vp.height,
z = subscene.par3d.zoom;
if (far < 0.)
far = 1.;
if (near < far/100.)
near = far/100.;
hlen = t*near;
if (ortho) {
if (aspect > 1)
this.prMatrix.ortho(-hlen*aspect*z, hlen*aspect*z,
-hlen*z, hlen*z, near, far);
else
this.prMatrix.ortho(-hlen*z, hlen*z,
-hlen*z/aspect, hlen*z/aspect,
near, far);
} else {
if (aspect > 1)
this.prMatrix.frustum(-hlen*aspect*z, hlen*aspect*z,
-hlen*z, hlen*z, near, far);
else
this.prMatrix.frustum(-hlen*z, hlen*z,
-hlen*z/aspect, hlen*z/aspect,
near, far);
}
};
this.setmvMatrix = function(id) {
var observer = this.getObj(id).par3d.observer;
this.mvMatrix.makeIdentity();
this.setmodelMatrix(id);
this.mvMatrix.translate(-observer[0], -observer[1], -observer[2]);
};
this.setmodelMatrix = function(id) {
var subscene = this.getObj(id),
embedding = subscene.embeddings.model;
if (embedding !== "inherit") {
var scale = subscene.par3d.scale,
bbox = subscene.par3d.bbox,
center = [(bbox[0]+bbox[1])/2,
(bbox[2]+bbox[3])/2,
(bbox[4]+bbox[5])/2];
this.mvMatrix.translate(-center[0], -center[1], -center[2]);
this.mvMatrix.scale(scale[0], scale[1], scale[2]);
this.mvMatrix.multRight( subscene.par3d.userMatrix );
}
if (embedding !== "replace")
this.setmodelMatrix(subscene.parent);
};
this.setnormMatrix = function(subsceneid) {
var self = this,
recurse = function(id) {
var sub = self.getObj(id),
embedding = sub.embeddings.model;
if (embedding !== "inherit") {
var scale = sub.par3d.scale;
self.normMatrix.scale(1/scale[0], 1/scale[1], 1/scale[2]);
self.normMatrix.multRight(sub.par3d.userMatrix);
}
if (embedding !== "replace")
recurse(sub.parent);
};
self.normMatrix.makeIdentity();
recurse(subsceneid);
};
this.setprmvMatrix = function() {
this.prmvMatrix = new CanvasMatrix4( this.mvMatrix );
this.prmvMatrix.multRight( this.prMatrix );
};
this.countClipplanes = function() {
return this.countObjs("clipplanes");
};
this.countLights = function() {
return this.countObjs("light");
};
this.countObjs = function(type) {
var self = this,
bound = 0;
Object.keys(this.scene.objects).forEach(
function(key) {
if (self.getObj(parseInt(key, 10)).type === type)
bound = bound + 1;
});
return bound;
};
this.initSubscene = function(id) {
var sub = this.getObj(id),
i, obj;
if (sub.type !== "subscene")
return;
sub.par3d.userMatrix = this.toCanvasMatrix4(sub.par3d.userMatrix);
sub.par3d.listeners = [].concat(sub.par3d.listeners);
sub.backgroundId = undefined;
sub.subscenes = [];
sub.clipplanes = [];
sub.transparent = [];
sub.opaque = [];
sub.lights = [];
for (i=0; i < sub.objects.length; i++) {
obj = this.getObj(sub.objects[i]);
if (typeof obj === "undefined") {
sub.objects.splice(i, 1);
i--;
} else if (obj.type === "background")
sub.backgroundId = obj.id;
else
sub[this.whichList(obj.id)].push(obj.id);
}
};
this.copyObj = function(id, reuse) {
var obj = this.getObj(id),
prev = document.getElementById(reuse);
if (prev !== null) {
prev = prev.rglinstance;
var
prevobj = prev.getObj(id),
fields = ["flags", "type",
"colors", "vertices", "centers",
"normals", "offsets",
"texts", "cex", "family", "font", "adj",
"material",
"radii",
"texcoords",
"userMatrix", "ids",
"dim",
"par3d", "userMatrix",
"viewpoint", "finite"],
i;
for (i = 0; i < fields.length; i++) {
if (typeof prevobj[fields[i]] !== "undefined")
obj[fields[i]] = prevobj[fields[i]];
}
} else
console.warn("copyObj failed");
};
this.planeUpdateTriangles = function(id, bbox) {
var perms = [[0,0,1], [1,2,2], [2,1,0]],
x, xrow, elem, A, d, nhits, i, j, k, u, v, w, intersect, which, v0, v2, vx, reverse,
face1 = [], face2 = [], normals = [],
obj = this.getObj(id),
nPlanes = obj.normals.length;
obj.bbox = bbox;
obj.vertices = [];
obj.initialized = false;
for (elem = 0; elem < nPlanes; elem++) {
//    Vertex Av = normal.getRecycled(elem);
x = [];
A = obj.normals[elem];
d = obj.offsets[elem][0];
nhits = 0;
for (i=0; i<3; i++)
for (j=0; j<2; j++)
for (k=0; k<2; k++) {
u = perms[0][i];
v = perms[1][i];
w = perms[2][i];
if (A[w] !== 0.0) {
intersect = -(d + A[u]*bbox[j+2*u] + A[v]*bbox[k+2*v])/A[w];
if (bbox[2*w] < intersect && intersect < bbox[1+2*w]) {
xrow = [];
xrow[u] = bbox[j+2*u];
xrow[v] = bbox[k+2*v];
xrow[w] = intersect;
x.push(xrow);
face1[nhits] = j + 2*u;
face2[nhits] = k + 2*v;
nhits++;
}
}
}
if (nhits > 3) {
/* Re-order the intersections so the triangles work */
for (i=0; i<nhits-2; i++) {
which = 0; /* initialize to suppress warning */
for (j=i+1; j<nhits; j++) {
if (face1[i] == face1[j] || face1[i] == face2[j] ||
face2[i] == face1[j] || face2[i] == face2[j] ) {
which = j;
break;
}
}
if (which > i+1) {
this.swap(x, i+1, which);
this.swap(face1, i+1, which);
this.swap(face2, i+1, which);
}
}
}
if (nhits >= 3) {
/* Put in order so that the normal points out the FRONT of the faces */
v0 = [x[0][0] - x[1][0] , x[0][1] - x[1][1], x[0][2] - x[1][2]];
v2 = [x[2][0] - x[1][0] , x[2][1] - x[1][1], x[2][2] - x[1][2]];
/* cross-product */
vx = this.xprod(v0, v2);
reverse = this.dotprod(vx, A) > 0;
for (i=0; i<nhits-2; i++) {
obj.vertices.push(x[0]);
normals.push(A);
for (j=1; j<3; j++) {
obj.vertices.push(x[i + (reverse ? 3-j : j)]);
normals.push(A);
}
}
}
}
obj.pnormals = normals;
};
this.initObj = function(id) {
var obj = this.getObj(id),
flags = obj.flags,
type = obj.type,
is_indexed = flags & this.f_is_indexed,
is_lit = flags & this.f_is_lit,
has_texture = flags & this.f_has_texture,
fixed_quads = flags & this.f_fixed_quads,
depth_sort = flags & this.f_depth_sort,
sprites_3d = flags & this.f_sprites_3d,
sprite_3d = flags & this.f_sprite_3d,
fixed_size = flags & this.f_fixed_size,
is_twosided = (flags & this.f_is_twosided) > 0,
gl = this.gl || this.initGL(),
texinfo, drawtype, nclipplanes, f, nrows,
i,j,v,v1,v2, mat, uri, matobj, pass, pmode,
dim, nx, nz, attr;
if (typeof id !== "number") {
this.alertOnce("initObj id is "+typeof id);
}
obj.initialized = true;
if (type === "bboxdeco" || type === "subscene")
return;
if (type === "light") {
obj.ambient = new Float32Array(obj.colors[0].slice(0,3));
obj.diffuse = new Float32Array(obj.colors[1].slice(0,3));
obj.specular = new Float32Array(obj.colors[2].slice(0,3));
obj.lightDir = new Float32Array(obj.vertices[0]);
return;
}
if (type === "clipplanes") {
obj.vClipplane = this.flatten(this.cbind(obj.normals, obj.offsets));
return;
}
if (type == "background" && typeof obj.ids !== "undefined") {
obj.quad = this.flatten([].concat(obj.ids));
return;
}
if (typeof obj.vertices === "undefined")
obj.vertices = [];
v = obj.vertices;
obj.vertexCount = v.length;
if (!obj.vertexCount) return;
if (is_twosided) {
if (typeof obj.userAttributes === "undefined")
obj.userAttributes = {};
v1 = Array(v.length);
v2 = Array(v.length);  
if (obj.type == "triangles" || obj.type == "quads") {
if (obj.type == "triangles")
nrow = 3;
else
nrow = 4;
for (i=0; i<Math.floor(v.length/nrow); i++)
for (j=0; j<nrow; j++) {
v1[nrow*i + j] = v[nrow*i + ((j+1) % nrow)];
v2[nrow*i + j] = v[nrow*i + ((j+2) % nrow)];
}
} else if (obj.type == "surface") {
dim = obj.dim[0];
nx = dim[0];
nz = dim[1];
for (j=0; j<nx; j++) {
for (i=0; i<nz; i++) {
if (i+1 < nz && j+1 < nx) { 
v2[j + nx*i] = v[j + nx*(i+1)];
v1[j + nx*i] = v[j+1 + nx*(i+1)];
} else if (i+1 < nz) {
v2[j + nx*i] = v[j-1 + nx*i];
v1[j + nx*i] = v[j + nx*(i+1)];             
} else {
v2[j + nx*i] = v[j + nx*(i-1)];
v1[j + nx*i] = v[j-1 + nx*(i-1)];
}
}
}
}
obj.userAttributes.aPos1 = v1;
obj.userAttributes.aPos2 = v2;
}
if (!sprites_3d) {
if (gl.isContextLost()) return;
obj.prog = gl.createProgram();
gl.attachShader(obj.prog, this.getShader( gl.VERTEX_SHADER,
this.getVertexShader(id) ));
gl.attachShader(obj.prog, this.getShader( gl.FRAGMENT_SHADER,
this.getFragmentShader(id) ));
//  Force aPos to location 0, aCol to location 1
gl.bindAttribLocation(obj.prog, 0, "aPos");
gl.bindAttribLocation(obj.prog, 1, "aCol");
gl.linkProgram(obj.prog);
var linked = gl.getProgramParameter(obj.prog, gl.LINK_STATUS);
if (!linked) {
// An error occurred while linking
var lastError = gl.getProgramInfoLog(obj.prog);
console.warn("Error in program linking:" + lastError);
gl.deleteProgram(obj.prog);
return;
}
}
if (type === "text") {
texinfo = this.drawTextToCanvas(obj.texts,
this.flatten(obj.cex),
this.flatten(obj.family),
this.flatten(obj.family));
}
if (fixed_quads && !sprites_3d) {
obj.ofsLoc = gl.getAttribLocation(obj.prog, "aOfs");
}
if (sprite_3d) {
obj.origLoc = gl.getUniformLocation(obj.prog, "uOrig");
obj.sizeLoc = gl.getUniformLocation(obj.prog, "uSize");
obj.usermatLoc = gl.getUniformLocation(obj.prog, "usermat");
}
if (has_texture || type == "text") {
obj.texture = gl.createTexture();
obj.texLoc = gl.getAttribLocation(obj.prog, "aTexcoord");
obj.sampler = gl.getUniformLocation(obj.prog, "uSampler");
}
if (has_texture) {
mat = obj.material;
if (typeof mat.uri !== "undefined")
uri = mat.uri;
else if (typeof mat.uriElementId === "undefined") {
matobj = this.getObj(mat.uriId);
if (typeof matobj !== "undefined") {
uri = matobj.material.uri;
} else {
uri = "";
}
} else
uri = document.getElementById(mat.uriElementId).rglinstance.getObj(mat.uriId).material.uri;
this.loadImageToTexture(uri, obj.texture);
}
if (type === "text") {
this.handleLoadedTexture(obj.texture, this.textureCanvas);
}
var stride = 3, nc, cofs, nofs, radofs, oofs, tofs, vnew;
nc = obj.colorCount = obj.colors.length;
if (nc > 1) {
cofs = stride;
stride = stride + 4;
v = this.cbind(v, obj.colors);
} else {
cofs = -1;
obj.onecolor = this.flatten(obj.colors);
}
if (typeof obj.normals !== "undefined") {
nofs = stride;
stride = stride + 3;
v = this.cbind(v, typeof obj.pnormals !== "undefined" ? obj.pnormals : obj.normals);
} else
nofs = -1;
if (typeof obj.radii !== "undefined") {
radofs = stride;
stride = stride + 1;
// FIXME:  always concat the radii?
if (obj.radii.length === v.length) {
v = this.cbind(v, obj.radii);
} else if (obj.radii.length === 1) {
v = v.map(function(row, i, arr) { return row.concat(obj.radii[0]);});
}
} else
radofs = -1;
if (type == "sprites" && !sprites_3d) {
tofs = stride;
stride += 2;
oofs = stride;
stride += 2;
vnew = new Array(4*v.length);
var rescale = fixed_size ? 72 : 1,
size = obj.radii, s = rescale*size[0]/2;
for (i=0; i < v.length; i++) {
if (size.length > 1)
s = rescale*size[i]/2;
vnew[4*i]  = v[i].concat([0,0,-s,-s]);
vnew[4*i+1]= v[i].concat([1,0, s,-s]);
vnew[4*i+2]= v[i].concat([1,1, s, s]);
vnew[4*i+3]= v[i].concat([0,1,-s, s]);
}
v = vnew;
obj.vertexCount = v.length;
} else if (type === "text") {
tofs = stride;
stride += 2;
oofs = stride;
stride += 2;
vnew = new Array(4*v.length);
for (i=0; i < v.length; i++) {
vnew[4*i]  = v[i].concat([0,-0.5]).concat(obj.adj[0]);
vnew[4*i+1]= v[i].concat([1,-0.5]).concat(obj.adj[0]);
vnew[4*i+2]= v[i].concat([1, 1.5]).concat(obj.adj[0]);
vnew[4*i+3]= v[i].concat([0, 1.5]).concat(obj.adj[0]);
for (j=0; j < 4; j++) {
v1 = vnew[4*i+j];
v1[tofs+2] = 2*(v1[tofs]-v1[tofs+2])*texinfo.widths[i];
v1[tofs+3] = 2*(v1[tofs+1]-v1[tofs+3])*texinfo.textHeights[i];
v1[tofs] *= texinfo.widths[i]/texinfo.canvasX;
v1[tofs+1] = 1.0-(texinfo.offsets[i] -
v1[tofs+1]*texinfo.textHeights[i])/texinfo.canvasY;
vnew[4*i+j] = v1;
}
}
v = vnew;
obj.vertexCount = v.length;
} else if (typeof obj.texcoords !== "undefined") {
tofs = stride;
stride += 2;
oofs = -1;
v = this.cbind(v, obj.texcoords);
} else {
tofs = -1;
oofs = -1;
}
if (typeof obj.userAttributes !== "undefined") {
obj.userAttribOffsets = {};
obj.userAttribLocations = {};
obj.userAttribSizes = {};
for (attr in obj.userAttributes) {
obj.userAttribLocations[attr] = gl.getAttribLocation(obj.prog, attr);
if (obj.userAttribLocations[attr] >= 0) { // Attribute may not have been used
obj.userAttribOffsets[attr] = stride;
v = this.cbind(v, obj.userAttributes[attr]);
stride = v[0].length;
obj.userAttribSizes[attr] = stride - obj.userAttribOffsets[attr];
}
}
}
if (typeof obj.userUniforms !== "undefined") {
obj.userUniformLocations = {};
for (attr in obj.userUniforms) 
obj.userUniformLocations[attr] = gl.getUniformLocation(obj.prog, attr);
}
if (stride !== v[0].length) {
this.alertOnce("problem in stride calculation");
}
obj.vOffsets = {vofs:0, cofs:cofs, nofs:nofs, radofs:radofs, oofs:oofs, tofs:tofs, stride:stride};
obj.values = new Float32Array(this.flatten(v));
if (sprites_3d) {
obj.userMatrix = new CanvasMatrix4(obj.userMatrix);
obj.objects = this.flatten([].concat(obj.ids));
is_lit = false;
}
if (is_lit && !fixed_quads) {
obj.normLoc = gl.getAttribLocation(obj.prog, "aNorm");
}
nclipplanes = this.countClipplanes();
if (nclipplanes && !sprites_3d) {
obj.clipLoc = [];
for (i=0; i < nclipplanes; i++)
obj.clipLoc[i] = gl.getUniformLocation(obj.prog,"vClipplane" + i);
}
if (is_lit) {
obj.emissionLoc = gl.getUniformLocation(obj.prog, "emission");
obj.emission = new Float32Array(this.stringToRgb(this.getMaterial(id, "emission")));
obj.shininessLoc = gl.getUniformLocation(obj.prog, "shininess");
obj.shininess = this.getMaterial(id, "shininess");
obj.nlights = this.countLights();
obj.ambientLoc = [];
obj.ambient = new Float32Array(this.stringToRgb(this.getMaterial(id, "ambient")));
obj.specularLoc = [];
obj.specular = new Float32Array(this.stringToRgb(this.getMaterial(id, "specular")));
obj.diffuseLoc = [];
obj.lightDirLoc = [];
obj.viewpointLoc = [];
obj.finiteLoc = [];
for (i=0; i < obj.nlights; i++) {
obj.ambientLoc[i] = gl.getUniformLocation(obj.prog, "ambient" + i);
obj.specularLoc[i] = gl.getUniformLocation(obj.prog, "specular" + i);
obj.diffuseLoc[i] = gl.getUniformLocation(obj.prog, "diffuse" + i);
obj.lightDirLoc[i] = gl.getUniformLocation(obj.prog, "lightDir" + i);
obj.viewpointLoc[i] = gl.getUniformLocation(obj.prog, "viewpoint" + i);
obj.finiteLoc[i] = gl.getUniformLocation(obj.prog, "finite" + i);
}
}
if (is_indexed) {
obj.f = Array(2);
for (pass = 0; pass < is_twosided + 1; pass++) {
if (type === "triangles" || type === "quads" || type === "surface")
pmode = this.getMaterial(id, (pass === 0) ? "front" : "back");
else pmode = "filled";
if (pmode === "culled")
continue;
if (pmode === "points") {
nrows = obj.vertexCount;
f = Array(nrows);
for (i=0; i < nrows; i++)
f[i] = i;
} else if ((type === "quads" || type === "text" ||
type === "sprites") && !sprites_3d) {
nrows = Math.floor(obj.vertexCount/4);
if (pmode === "filled") {
f = Array(6*nrows);
for (i=0; i < nrows; i++) {
f[6*i] = 4*i;
f[6*i+1] = 4*i + 1;
f[6*i+2] = 4*i + 2;
f[6*i+3] = 4*i;
f[6*i+4] = 4*i + 2;
f[6*i+5] = 4*i + 3;
}
} else {
f = Array(8*nrows);
for (i=0; i < nrows; i++) {
f[8*i] = 4*i;
f[8*i+1] = 4*i + 1;
f[8*i+2] = 4*i + 1;
f[8*i+3] = 4*i + 2;
f[8*i+4] = 4*i + 2;
f[8*i+5] = 4*i + 3;
f[8*i+6] = 4*i + 3;
f[8*i+7] = 4*i;
}
}
} else if (type === "triangles") {
nrows = Math.floor(obj.vertexCount/3);
if (pmode === "filled") {
f = Array(3*nrows);
for (i=0; i < f.length; i++) {
f[i] = i;
}
} else if (pmode === "lines") {
f = Array(6*nrows);
for (i=0; i < nrows; i++) {
f[6*i] = 3*i;
f[6*i + 1] = 3*i + 1;
f[6*i + 2] = 3*i + 1;
f[6*i + 3] = 3*i + 2;
f[6*i + 4] = 3*i + 2;  
f[6*i + 5] = 3*i;         
}
}
} else if (type === "spheres") {
nrows = obj.vertexCount;
f = Array(nrows);
for (i=0; i < f.length; i++) {
f[i] = i;
}
} else if (type === "surface") {
dim = obj.dim[0];
nx = dim[0];
nz = dim[1];
if (pmode === "filled") {
f = [];
for (j=0; j<nx-1; j++) {
for (i=0; i<nz-1; i++) {
f.push(j + nx*i,
j + nx*(i+1),
j + 1 + nx*(i+1),
j + nx*i,
j + 1 + nx*(i+1),
j + 1 + nx*i);
} 
}
} else if (pmode === "lines") {
f = [];
for (j=0; j<nx; j++) {
for (i=0; i<nz; i++) {
if (i+1 < nz)
f.push(j + nx*i,
j + nx*(i+1));
if (j+1 < nx)
f.push(j + nx*i,
j+1 + nx*i);
}
}
}
}
obj.f[pass] = new Uint16Array(f);
if (depth_sort) {
drawtype = "DYNAMIC_DRAW";
} else {
drawtype = "STATIC_DRAW";
}
}
}
if (type !== "spheres" && !sprites_3d) {
obj.buf = gl.createBuffer();
gl.bindBuffer(gl.ARRAY_BUFFER, obj.buf);
gl.bufferData(gl.ARRAY_BUFFER, obj.values, gl.STATIC_DRAW); //
}
if (is_indexed && type !== "spheres" && !sprites_3d) {
obj.ibuf = Array(is_twosided + 1);
obj.ibuf[0] = gl.createBuffer();
gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, obj.ibuf[0]);
gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, obj.f[0], gl[drawtype]);
if (is_twosided) {
obj.ibuf[1] = gl.createBuffer();
gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, obj.ibuf[1]);
gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, obj.f[1], gl[drawtype]);
}
}
if (!sprites_3d) {
obj.mvMatLoc = gl.getUniformLocation(obj.prog, "mvMatrix");
obj.prMatLoc = gl.getUniformLocation(obj.prog, "prMatrix");
}
if (fixed_size) {
obj.textScaleLoc = gl.getUniformLocation(obj.prog, "textScale");
}
if (is_lit && !sprites_3d) {
obj.normMatLoc = gl.getUniformLocation(obj.prog, "normMatrix");
}
if (is_twosided) {
obj.frontLoc = gl.getUniformLocation(obj.prog, "front");
}
};
this.setDepthTest = function(id) {
var gl = this.gl || this.initGL(),
tests = {never: gl.NEVER,
less:  gl.LESS,
equal: gl.EQUAL,
lequal:gl.LEQUAL,
greater: gl.GREATER,
notequal: gl.NOTEQUAL,
gequal: gl.GEQUAL,
always: gl.ALWAYS},
test = tests[this.getMaterial(id, "depth_test")];
gl.depthFunc(test);
};
this.mode4type = {points : "POINTS",
linestrip : "LINE_STRIP",
abclines : "LINES",
lines : "LINES",
sprites : "TRIANGLES",
planes : "TRIANGLES",
text : "TRIANGLES",
quads : "TRIANGLES",
surface : "TRIANGLES",
triangles : "TRIANGLES"};
this.drawObj = function(id, subsceneid) {
var obj = this.getObj(id),
subscene = this.getObj(subsceneid),
flags = obj.flags,
type = obj.type,
is_indexed = flags & this.f_is_indexed,
is_lit = flags & this.f_is_lit,
has_texture = flags & this.f_has_texture,
fixed_quads = flags & this.f_fixed_quads,
depth_sort = flags & this.f_depth_sort,
sprites_3d = flags & this.f_sprites_3d,
sprite_3d = flags & this.f_sprite_3d,
is_lines = flags & this.f_is_lines,
is_points = flags & this.f_is_points,
fixed_size = flags & this.f_fixed_size,
is_twosided = (flags & this.f_is_twosided) > 0,
gl = this.gl || this.initGL(),
mat,
sphereMV, baseofs, ofs, sscale, i, count, light,
faces, pass, mode, pmode, attr,
depthsort = function(i,j) { return depths[j] - depths[i]; };
if (typeof id !== "number") {
this.alertOnce("drawObj id is "+typeof id);
}
if (type === "planes") {
if (obj.bbox !== subscene.par3d.bbox || !obj.initialized) {
this.planeUpdateTriangles(id, subscene.par3d.bbox);
}
}
if (!obj.initialized)
this.initObj(id);
if (type === "clipplanes") {
count = obj.offsets.length;
var IMVClip = [];
for (i=0; i < count; i++) {
IMVClip[i] = this.multMV(this.invMatrix, obj.vClipplane.slice(4*i, 4*(i+1)));
}
obj.IMVClip = IMVClip;
return;
}
if (type === "light" || type === "bboxdeco" || !obj.vertexCount)
return;
this.setDepthTest(id);
if (sprites_3d) {
var norigs = obj.vertices.length,
savenorm = new CanvasMatrix4(this.normMatrix);
this.origs = obj.vertices;
this.usermat = new Float32Array(obj.userMatrix.getAsArray());
this.radii = obj.radii;
this.normMatrix = subscene.spriteNormmat;
for (this.iOrig=0; this.iOrig < norigs; this.iOrig++) {
for (i=0; i < obj.objects.length; i++) {
this.drawObj(obj.objects[i], subsceneid);
}
}
this.normMatrix = savenorm;
return;
} else {
gl.useProgram(obj.prog);
}
if (sprite_3d) {
gl.uniform3fv(obj.origLoc, new Float32Array(this.origs[this.iOrig]));
if (this.radii.length > 1) {
gl.uniform1f(obj.sizeLoc, this.radii[this.iOrig][0]);
} else {
gl.uniform1f(obj.sizeLoc, this.radii[0][0]);
}
gl.uniformMatrix4fv(obj.usermatLoc, false, this.usermat);
}
if (type === "spheres") {
gl.bindBuffer(gl.ARRAY_BUFFER, this.sphere.buf);
} else {
gl.bindBuffer(gl.ARRAY_BUFFER, obj.buf);
}
gl.uniformMatrix4fv( obj.prMatLoc, false, new Float32Array(this.prMatrix.getAsArray()) );
gl.uniformMatrix4fv( obj.mvMatLoc, false, new Float32Array(this.mvMatrix.getAsArray()) );
var clipcheck = 0,
clipplaneids = subscene.clipplanes,
clip, j;
for (i=0; i < clipplaneids.length; i++) {
clip = this.getObj(clipplaneids[i]);
for (j=0; j < clip.offsets.length; j++) {
gl.uniform4fv(obj.clipLoc[clipcheck + j], clip.IMVClip[j]);
}
clipcheck += clip.offsets.length;
}
if (typeof obj.clipLoc !== "undefined")
for (i=clipcheck; i < obj.clipLoc.length; i++)
gl.uniform4f(obj.clipLoc[i], 0,0,0,0);
if (is_lit) {
gl.uniformMatrix4fv( obj.normMatLoc, false, new Float32Array(this.normMatrix.getAsArray()) );
gl.uniform3fv( obj.emissionLoc, obj.emission);
gl.uniform1f( obj.shininessLoc, obj.shininess);
for (i=0; i < subscene.lights.length; i++) {
light = this.getObj(subscene.lights[i]);
gl.uniform3fv( obj.ambientLoc[i], this.componentProduct(light.ambient, obj.ambient));
gl.uniform3fv( obj.specularLoc[i], this.componentProduct(light.specular, obj.specular));
gl.uniform3fv( obj.diffuseLoc[i], light.diffuse);
gl.uniform3fv( obj.lightDirLoc[i], light.lightDir);
gl.uniform1i( obj.viewpointLoc[i], light.viewpoint);
gl.uniform1i( obj.finiteLoc[i], light.finite);
}
for (i=subscene.lights.length; i < obj.nlights; i++) {
gl.uniform3f( obj.ambientLoc[i], 0,0,0);
gl.uniform3f( obj.specularLoc[i], 0,0,0);
gl.uniform3f( obj.diffuseLoc[i], 0,0,0);
}
}
if (fixed_size) {
gl.uniform2f( obj.textScaleLoc, 0.75/this.vp.width, 0.75/this.vp.height);
}
gl.enableVertexAttribArray( this.posLoc );
var nc = obj.colorCount;
count = obj.vertexCount;
if (type === "spheres") {
subscene = this.getObj(subsceneid);
var scale = subscene.par3d.scale,
scount = count;
gl.vertexAttribPointer(this.posLoc,  3, gl.FLOAT, false, 4*this.sphere.vOffsets.stride,  0);
gl.enableVertexAttribArray(obj.normLoc );
gl.vertexAttribPointer(obj.normLoc,  3, gl.FLOAT, false, 4*this.sphere.vOffsets.stride,  0);
gl.disableVertexAttribArray( this.colLoc );
var sphereNorm = new CanvasMatrix4();
sphereNorm.scale(scale[0], scale[1], scale[2]);
sphereNorm.multRight(this.normMatrix);
gl.uniformMatrix4fv( obj.normMatLoc, false, new Float32Array(sphereNorm.getAsArray()) );
if (nc == 1) {
gl.vertexAttrib4fv( this.colLoc, new Float32Array(obj.onecolor));
}
if (has_texture) {
gl.enableVertexAttribArray( obj.texLoc );
gl.vertexAttribPointer(obj.texLoc, 2, gl.FLOAT, false, 4*this.sphere.vOffsets.stride, 
4*this.sphere.vOffsets.tofs);
gl.activeTexture(gl.TEXTURE0);
gl.bindTexture(gl.TEXTURE_2D, obj.texture);
gl.uniform1i( obj.sampler, 0);
}
for (i = 0; i < scount; i++) {
sphereMV = new CanvasMatrix4();
if (depth_sort) {
baseofs = faces[i]*obj.vOffsets.stride;
} else {
baseofs = i*obj.vOffsets.stride;
}
ofs = baseofs + obj.vOffsets.radofs;
sscale = obj.values[ofs];
sphereMV.scale(sscale/scale[0], sscale/scale[1], sscale/scale[2]);
sphereMV.translate(obj.values[baseofs],
obj.values[baseofs+1],
obj.values[baseofs+2]);
sphereMV.multRight(this.mvMatrix);
gl.uniformMatrix4fv( obj.mvMatLoc, false, new Float32Array(sphereMV.getAsArray()) );
if (nc > 1) {
ofs = baseofs + obj.vOffsets.cofs;
gl.vertexAttrib4f( this.colLoc, obj.values[ofs],
obj.values[ofs+1],
obj.values[ofs+2],
obj.values[ofs+3] );
}
gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.sphere.ibuf);
gl.drawElements(gl.TRIANGLES, this.sphere.sphereCount, gl.UNSIGNED_SHORT, 0);
}
return;
} else {
if (obj.colorCount === 1) {
gl.disableVertexAttribArray( this.colLoc );
gl.vertexAttrib4fv( this.colLoc, new Float32Array(obj.onecolor));
} else {
gl.enableVertexAttribArray( this.colLoc );
gl.vertexAttribPointer(this.colLoc, 4, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.cofs);
}
}
if (is_lit && obj.vOffsets.nofs > 0) {
gl.enableVertexAttribArray( obj.normLoc );
gl.vertexAttribPointer(obj.normLoc, 3, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.nofs);
}
if (has_texture || type === "text") {
gl.enableVertexAttribArray( obj.texLoc );
gl.vertexAttribPointer(obj.texLoc, 2, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.tofs);
gl.activeTexture(gl.TEXTURE0);
gl.bindTexture(gl.TEXTURE_2D, obj.texture);
gl.uniform1i( obj.sampler, 0);
}
if (fixed_quads) {
gl.enableVertexAttribArray( obj.ofsLoc );
gl.vertexAttribPointer(obj.ofsLoc, 2, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.oofs);
}
if (typeof obj.userAttributes !== "undefined") {
for (attr in obj.userAttribSizes) {  // Not all attributes may have been used
gl.enableVertexAttribArray( obj.userAttribLocations[attr] );
gl.vertexAttribPointer( obj.userAttribLocations[attr], obj.userAttribSizes[attr],
gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.userAttribOffsets[attr]);
}
}
if (typeof obj.userUniforms !== "undefined") {
for (attr in obj.userUniformLocations) {
var loc = obj.userUniformLocations[attr];
if (loc !== null) {
var uniform = obj.userUniforms[attr];
if (typeof uniform.length === "undefined")
gl.uniform1f(loc, uniform);
else if (typeof uniform[0].length === "undefined") {
uniform = new Float32Array(uniform);
switch(uniform.length) {
case 2: gl.uniform2fv(loc, uniform); break;
case 3: gl.uniform3fv(loc, uniform); break;
case 4: gl.uniform4fv(loc, uniform); break;
default: console.warn("bad uniform length");
}
} else if (uniform.length == 4 && uniform[0].length == 4) 
gl.uniformMatrix4fv(loc, false, new Float32Array(uniform.getAsArray()));
else
console.warn("unsupported uniform matrix");
}
}
}
for (pass = 0; pass < is_twosided + 1; pass++) {
if (type === "triangles" || type === "quads" || type === "surface")
pmode = this.getMaterial(id, (pass === 0) ? "front" : "back");
else pmode = "filled";
if (pmode === "culled")
continue;
mode = this.mode4type[type];      
if (depth_sort && pmode == "filled") {// Don't try depthsorting on wireframe or points
var nfaces = obj.centers.length,
z, w, frowsize;
frowsize = Math.floor(obj.f[pass].length/nfaces);
var depths = new Float32Array(nfaces);
faces = new Array(nfaces);
for(i=0; i<nfaces; i++) {
z = this.prmvMatrix.m13*obj.centers[3*i] +
this.prmvMatrix.m23*obj.centers[3*i+1] +
this.prmvMatrix.m33*obj.centers[3*i+2] +
this.prmvMatrix.m43;
w = this.prmvMatrix.m14*obj.centers[3*i] +
this.prmvMatrix.m24*obj.centers[3*i+1] +
this.prmvMatrix.m34*obj.centers[3*i+2] +
this.prmvMatrix.m44;
depths[i] = z/w;
faces[i] = i;
}
faces.sort(depthsort);
if (type !== "spheres") {
var f = new Uint16Array(obj.f[pass].length);
for (i=0; i<nfaces; i++) {
for (j=0; j<frowsize; j++) {
f[frowsize*i + j] = obj.f[pass][frowsize*faces[i] + j];
}
}
gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, obj.ibuf[pass]);
gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, f, gl.DYNAMIC_DRAW);
}
}
if (is_twosided)
gl.uniform1i(obj.frontLoc, pass !== 0);
if (is_indexed && type !== "spheres") {
gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, obj.ibuf[pass]);
} else if (type === "spheres") {
//  FIX ME!
} 
if (type === "sprites" || type === "text" || type === "quads") {
count = count * 6/4;
} else if (type === "surface") {
count = obj.f[pass].length;
}
if (is_indexed) {
count = obj.f[pass].length;
if (pmode === "lines") {
mode = "LINES";
is_lines = true;
} else if (pmode === "points") {
mode = "POINTS";
}
}
if (is_lines) {
gl.lineWidth( this.getMaterial(id, "lwd") );
}
gl.vertexAttribPointer(this.posLoc,  3, gl.FLOAT, false, 4*obj.vOffsets.stride,  4*obj.vOffsets.vofs);
if (is_indexed) {
gl.drawElements(gl[mode], count, gl.UNSIGNED_SHORT, 0);
} else {
gl.drawArrays(gl[mode], 0, count);
}
}
};
this.drawBackground = function(id, subsceneid) {
var gl = this.gl || this.initGL(),
obj = this.getObj(id),
bg, i;
if (!obj.initialized)
this.initObj(id);
if (obj.colors.length) {
bg = obj.colors[0];
gl.clearColor(bg[0], bg[1], bg[2], bg[3]);
gl.depthMask(true);
gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
}
if (typeof obj.quad !== "undefined") {
this.prMatrix.makeIdentity();
this.mvMatrix.makeIdentity();
gl.disable(gl.BLEND);
gl.disable(gl.DEPTH_TEST);
gl.depthMask(false);
for (i=0; i < obj.quad.length; i++)
this.drawObj(obj.quad[i], subsceneid);
}
};
this.drawSubscene = function(subsceneid) {
var gl = this.gl || this.initGL(),
obj = this.getObj(subsceneid),
objects = this.scene.objects,
subids = obj.objects,
subscene_has_faces = false,
subscene_needs_sorting = false,
flags, i;
if (obj.par3d.skipRedraw)
return;
for (i=0; i < subids.length; i++) {
flags = objects[subids[i]].flags;
if (typeof flags !== "undefined") {
subscene_has_faces |= (flags & this.f_is_lit)
& !(flags & this.f_fixed_quads);
subscene_needs_sorting |= (flags & this.f_depth_sort);
}
}
this.setViewport(subsceneid);
if (typeof obj.backgroundId !== "undefined")
this.drawBackground(obj.backgroundId, subsceneid);
if (subids.length) {
this.setprMatrix(subsceneid);
this.setmvMatrix(subsceneid);
if (subscene_has_faces) {
this.setnormMatrix(subsceneid);
if ((obj.flags & this.f_sprites_3d) &&
typeof obj.spriteNormmat === "undefined") {
obj.spriteNormmat = new CanvasMatrix4(this.normMatrix);
}
}
if (subscene_needs_sorting)
this.setprmvMatrix();
gl.enable(gl.DEPTH_TEST);
gl.depthMask(true);
gl.disable(gl.BLEND);
var clipids = obj.clipplanes;
if (typeof clipids === "undefined") {
console.warn("bad clipids");
}
if (clipids.length > 0) {
this.invMatrix = new CanvasMatrix4(this.mvMatrix);
this.invMatrix.invert();
for (i = 0; i < clipids.length; i++)
this.drawObj(clipids[i], subsceneid);
}
subids = obj.opaque;
if (subids.length > 0) {
for (i = 0; i < subids.length; i++) {
this.drawObj(subids[i], subsceneid);
}
}
subids = obj.transparent;
if (subids.length > 0) {
gl.depthMask(false);
gl.blendFuncSeparate(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA,
gl.ONE, gl.ONE);
gl.enable(gl.BLEND);
for (i = 0; i < subids.length; i++) {
this.drawObj(subids[i], subsceneid);
}
}
subids = obj.subscenes;
for (i = 0; i < subids.length; i++) {
this.drawSubscene(subids[i]);
}
}
};
this.relMouseCoords = function(event) {
var totalOffsetX = 0,
totalOffsetY = 0,
currentElement = this.canvas;
do {
totalOffsetX += currentElement.offsetLeft;
totalOffsetY += currentElement.offsetTop;
currentElement = currentElement.offsetParent;
}
while(currentElement);
var canvasX = event.pageX - totalOffsetX,
canvasY = event.pageY - totalOffsetY;
return {x:canvasX, y:canvasY};
};
this.setMouseHandlers = function() {
var self = this, activeSubscene, handler,
handlers = {}, drag = 0;
handlers.rotBase = 0;
this.screenToVector = function(x, y) {
var viewport = this.getObj(activeSubscene).par3d.viewport,
width = viewport.width*this.canvas.width,
height = viewport.height*this.canvas.height,
radius = Math.max(width, height)/2.0,
cx = width/2.0,
cy = height/2.0,
px = (x-cx)/radius,
py = (y-cy)/radius,
plen = Math.sqrt(px*px+py*py);
if (plen > 1.e-6) {
px = px/plen;
py = py/plen;
}
var angle = (Math.SQRT2 - plen)/Math.SQRT2*Math.PI/2,
z = Math.sin(angle),
zlen = Math.sqrt(1.0 - z*z);
px = px * zlen;
py = py * zlen;
return [px, py, z];
};
handlers.trackballdown = function(x,y) {
var activeSub = this.getObj(activeSubscene),
activeModel = this.getObj(this.useid(activeSub.id, "model")),
i, l = activeModel.par3d.listeners;
handlers.rotBase = this.screenToVector(x, y);
this.saveMat = [];
for (i = 0; i < l.length; i++) {
activeSub = this.getObj(l[i]);
activeSub.saveMat = new CanvasMatrix4(activeSub.par3d.userMatrix);
}
};
handlers.trackballmove = function(x,y) {
var rotCurrent = this.screenToVector(x,y),
rotBase = handlers.rotBase,
dot = rotBase[0]*rotCurrent[0] +
rotBase[1]*rotCurrent[1] +
rotBase[2]*rotCurrent[2],
angle = Math.acos( dot/this.vlen(rotBase)/this.vlen(rotCurrent) )*180.0/Math.PI,
axis = this.xprod(rotBase, rotCurrent),
objects = this.scene.objects,
activeSub = this.getObj(activeSubscene),
activeModel = this.getObj(this.useid(activeSub.id, "model")),
l = activeModel.par3d.listeners,
i;
for (i = 0; i < l.length; i++) {
activeSub = this.getObj(l[i]);
activeSub.par3d.userMatrix.load(objects[l[i]].saveMat);
activeSub.par3d.userMatrix.rotate(angle, axis[0], axis[1], axis[2]);
}
this.drawScene();
};
handlers.trackballend = 0;
handlers.axisdown = function(x,y) {
handlers.rotBase = this.screenToVector(x, this.canvas.height/2);
var activeSub = this.getObj(activeSubscene),
activeModel = this.getObj(this.useid(activeSub.id, "model")),
i, l = activeModel.par3d.listeners;
for (i = 0; i < l.length; i++) {
activeSub = this.getObj(l[i]);
activeSub.saveMat = new CanvasMatrix4(activeSub.par3d.userMatrix);
}
};
handlers.axismove = function(x,y) {
var rotCurrent = this.screenToVector(x, this.canvas.height/2),
rotBase = handlers.rotBase,
angle = (rotCurrent[0] - rotBase[0])*180/Math.PI,
rotMat = new CanvasMatrix4();
rotMat.rotate(angle, handlers.axis[0], handlers.axis[1], handlers.axis[2]);
var activeSub = this.getObj(activeSubscene),
activeModel = this.getObj(this.useid(activeSub.id, "model")),
i, l = activeModel.par3d.listeners;
for (i = 0; i < l.length; i++) {
activeSub = this.getObj(l[i]);
activeSub.par3d.userMatrix.load(activeSub.saveMat);
activeSub.par3d.userMatrix.multLeft(rotMat);
}
this.drawScene();
};
handlers.axisend = 0;
handlers.y0zoom = 0;
handlers.zoom0 = 0;
handlers.zoomdown = function(x, y) {
var activeSub = this.getObj(activeSubscene),
activeProjection = this.getObj(this.useid(activeSub.id, "projection")),
i, l = activeProjection.par3d.listeners;
handlers.y0zoom = y;
for (i = 0; i < l.length; i++) {
activeSub = this.getObj(l[i]);
activeSub.zoom0 = Math.log(activeSub.par3d.zoom);
}
};
handlers.zoommove = function(x, y) {
var activeSub = this.getObj(activeSubscene),
activeProjection = this.getObj(this.useid(activeSub.id, "projection")),
i, l = activeProjection.par3d.listeners;
for (i = 0; i < l.length; i++) {
activeSub = this.getObj(l[i]);
activeSub.par3d.zoom = Math.exp(activeSub.zoom0 + (y-handlers.y0zoom)/this.canvas.height);
}
this.drawScene();
};
handlers.zoomend = 0;
handlers.y0fov = 0;
handlers.fovdown = function(x, y) {
handlers.y0fov = y;
var activeSub = this.getObj(activeSubscene),
activeProjection = this.getObj(this.useid(activeSub.id, "projection")),
i, l = activeProjection.par3d.listeners;
for (i = 0; i < l.length; i++) {
activeSub = this.getObj(l[i]);
activeSub.fov0 = activeSub.par3d.FOV;
}
};
handlers.fovmove = function(x, y) {
var activeSub = this.getObj(activeSubscene),
activeProjection = this.getObj(this.useid(activeSub.id, "projection")),
i, l = activeProjection.par3d.listeners;
for (i = 0; i < l.length; i++) {
activeSub = this.getObj(l[i]);
activeSub.par3d.FOV = Math.max(1, Math.min(179, activeSub.fov0 +
180*(y-handlers.y0fov)/this.canvas.height));
}
this.drawScene();
};
handlers.fovend = 0;
this.canvas.onmousedown = function ( ev ){
if (!ev.which) // Use w3c defns in preference to MS
switch (ev.button) {
case 0: ev.which = 1; break;
case 1:
case 4: ev.which = 2; break;
case 2: ev.which = 3;
}
drag = ["left", "middle", "right"][ev.which-1];
var coords = self.relMouseCoords(ev);
coords.y = self.canvas.height-coords.y;
activeSubscene = self.whichSubscene(coords);
var sub = self.getObj(activeSubscene), f;
handler = sub.par3d.mouseMode[drag];
switch (handler) {
case "xAxis":
handler = "axis";
handlers.axis = [1.0, 0.0, 0.0];
break;
case "yAxis":
handler = "axis";
handlers.axis = [0.0, 1.0, 0.0];
break;
case "zAxis":
handler = "axis";
handlers.axis = [0.0, 0.0, 1.0];
break;
}
f = handlers[handler + "down"];
if (f) {
coords = self.translateCoords(activeSubscene, coords);
f.call(self, coords.x, coords.y);
ev.preventDefault();
}
};
this.canvas.onmouseup = function ( ev ){
if ( drag === 0 ) return;
var f = handlers[handler + "up"];
if (f)
f();
drag = 0;
};
this.canvas.onmouseout = this.canvas.onmouseup;
this.canvas.onmousemove = function ( ev ) {
if ( drag === 0 ) return;
var f = handlers[handler + "move"];
if (f) {
var coords = self.relMouseCoords(ev);
coords.y = self.canvas.height - coords.y;
coords = self.translateCoords(activeSubscene, coords);
f.call(self, coords.x, coords.y);
}
};
handlers.wheelHandler = function(ev) {
var del = 1.02, i;
if (ev.shiftKey) del = 1.002;
var ds = ((ev.detail || ev.wheelDelta) > 0) ? del : (1 / del);
if (typeof activeSubscene === "undefined")
activeSubscene = self.scene.rootSubscene;
var activeSub = self.getObj(activeSubscene),
activeProjection = self.getObj(self.useid(activeSub.id, "projection")),
l = activeProjection.par3d.listeners;
for (i = 0; i < l.length; i++) {
activeSub = self.getObj(l[i]);
activeSub.par3d.zoom *= ds;
}
self.drawScene();
ev.preventDefault();
};
this.canvas.addEventListener("DOMMouseScroll", handlers.wheelHandler, false);
this.canvas.addEventListener("mousewheel", handlers.wheelHandler, false);
};
this.useid = function(subsceneid, type) {
var sub = this.getObj(subsceneid);
if (sub.embeddings[type] === "inherit")
return(this.useid(sub.parent, type));
else
return subsceneid;
};
this.inViewport = function(coords, subsceneid) {
var viewport = this.getObj(subsceneid).par3d.viewport,
x0 = coords.x - viewport.x*this.canvas.width,
y0 = coords.y - viewport.y*this.canvas.height;
return 0 <= x0 && x0 <= viewport.width*this.canvas.width &&
0 <= y0 && y0 <= viewport.height*this.canvas.height;
};
this.whichSubscene = function(coords) {
var self = this,
recurse = function(subsceneid) {
var subscenes = self.getChildSubscenes(subsceneid), i, id;
for (i=0; i < subscenes.length; i++) {
id = recurse(subscenes[i]);
if (typeof(id) !== "undefined")
return(id);
}
if (self.inViewport(coords, subsceneid))
return(subsceneid);
else
return undefined;
},
rootid = this.scene.rootSubscene,
result = recurse(rootid);
if (typeof(result) === "undefined")
result = rootid;
return result;
};
this.translateCoords = function(subsceneid, coords) {
var viewport = this.getObj(subsceneid).par3d.viewport;
return {x: coords.x - viewport.x*this.canvas.width,
y: coords.y - viewport.y*this.canvas.height};
};
this.initSphere = function() {
var verts = this.scene.sphereVerts, 
reuse = verts.reuse, result;
if (typeof reuse !== "undefined") {
var prev = document.getElementById(reuse).rglinstance.sphere;
result = {values: prev.values, vOffsets: prev.vOffsets, it: prev.it};
} else 
result = {values: new Float32Array(this.flatten(this.cbind(this.transpose(verts.vb),
this.transpose(verts.texcoords)))),
it: new Uint16Array(this.flatten(this.transpose(verts.it))),
vOffsets: {vofs:0, cofs:-1, nofs:-1, radofs:-1, oofs:-1, 
tofs:3, stride:5}};
result.sphereCount = result.it.length;
this.sphere = result;
};
this.initSphereGL = function() {
var gl = this.gl || this.initGL(), sphere = this.sphere;
if (gl.isContextLost()) return;
sphere.buf = gl.createBuffer();
gl.bindBuffer(gl.ARRAY_BUFFER, sphere.buf);
gl.bufferData(gl.ARRAY_BUFFER, sphere.values, gl.STATIC_DRAW);
sphere.ibuf = gl.createBuffer();
gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, sphere.ibuf);
gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, sphere.it, gl.STATIC_DRAW);
return;
};
this.initialize = function(el, x) {
this.textureCanvas = document.createElement("canvas");
this.textureCanvas.style.display = "block";
this.scene = x;
this.normMatrix = new CanvasMatrix4();
this.saveMat = {};
this.distance = null;
this.posLoc = 0;
this.colLoc = 1;
if (el) {
el.rglinstance = this;
this.el = el;
this.webGLoptions = el.rglinstance.scene.webGLoptions;
this.initCanvas();
}
};
this.restartCanvas = function() {
var newcanvas = document.createElement("canvas");
newcanvas.width = this.el.width;
newcanvas.height = this.el.height;
newcanvas.addEventListener("webglcontextrestored",
this.onContextRestored, false);
newcanvas.addEventListener("webglcontextlost",
this.onContextLost, false);            
while (this.el.firstChild) {
this.el.removeChild(this.el.firstChild);
}
this.el.appendChild(newcanvas);
this.canvas = newcanvas;
this.gl = null;         
};
this.initCanvas = function() {
this.restartCanvas();
var objs = this.scene.objects,
self = this;
Object.keys(objs).forEach(function(key){
var id = parseInt(key, 10),
obj = self.getObj(id);
if (typeof obj.reuse !== "undefined")
self.copyObj(id, obj.reuse);
});
Object.keys(objs).forEach(function(key){
self.initSubscene(parseInt(key, 10));
});
this.setMouseHandlers();      
this.initSphere();
this.onContextRestored = function(event) {
self.initGL();
self.drawScene();
// console.log("restored context for "+self.scene.rootSubscene);
};
this.onContextLost = function(event) {
if (!self.drawing)
self.restartCanvas();
event.preventDefault();
};
this.initGL0();
lazyLoadScene = function() {
if (self.isInBrowserViewport()) {
if (!self.gl) {
self.initGL();
}
self.drawScene();
}
};
window.addEventListener("DOMContentLoaded", lazyLoadScene, false);
window.addEventListener("load", lazyLoadScene, false);
window.addEventListener("resize", lazyLoadScene, false);
window.addEventListener("scroll", lazyLoadScene, false);
};
/* this is only used by writeWebGL; rglwidget has
no debug element and does the drawing in rglwidget.js */
this.start = function() {
if (typeof this.prefix !== "undefined") {
this.debugelement = document.getElementById(this.prefix + "debug");
this.debug("");
}
this.drag = 0;
this.drawScene();
};
this.debug = function(msg, img) {
if (typeof this.debugelement !== "undefined" && this.debugelement !== null) {
this.debugelement.innerHTML = msg;
if (typeof img !== "undefined") {
this.debugelement.insertBefore(img, this.debugelement.firstChild);
}
} else if (msg !== "")
alert(msg);
};
this.getSnapshot = function() {
var img;
if (typeof this.scene.snapshot !== "undefined") {
img = document.createElement("img");
img.src = this.scene.snapshot;
img.alt = "Snapshot";
}
return img;
};
this.initGL0 = function() {
if (!window.WebGLRenderingContext){
alert("Your browser does not support WebGL. See http://get.webgl.org");
return;
}
};
this.isInBrowserViewport = function() {
var rect = this.canvas.getBoundingClientRect(),
windHeight = (window.innerHeight || document.documentElement.clientHeight),
windWidth = (window.innerWidth || document.documentElement.clientWidth);
return (
rect.top >= -windHeight &&
rect.left >= -windWidth &&
rect.bottom <= 2*windHeight &&
rect.right <= 2*windWidth);
};
this.initGL = function() {
var self = this;
if (this.gl) {
if (!this.drawing && this.gl.isContextLost())
this.restartCanvas();
else
return this.gl;
}
// if (!this.isInBrowserViewport()) return; Return what??? At this point we know this.gl is null.
this.canvas.addEventListener("webglcontextrestored",
this.onContextRestored, false);
this.canvas.addEventListener("webglcontextlost",
this.onContextLost, false);      
this.gl = this.canvas.getContext("webgl", this.webGLoptions) ||
this.canvas.getContext("experimental-webgl", this.webGLoptions);
var save = this.startDrawing();
this.initSphereGL(); 
Object.keys(this.scene.objects).forEach(function(key){
self.initObj(parseInt(key, 10));
});
this.stopDrawing(save);
return this.gl;
};
this.resize = function(el) {
this.canvas.width = el.width;
this.canvas.height = el.height;
};
this.drawScene = function() {
var gl = this.gl || this.initGL(),
save = this.startDrawing();
gl.enable(gl.DEPTH_TEST);
gl.depthFunc(gl.LEQUAL);
gl.clearDepth(1.0);
gl.clearColor(1,1,1,1);
gl.depthMask(true); // Must be true before clearing depth buffer
gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
this.drawSubscene(this.scene.rootSubscene);
this.stopDrawing(save);
};
this.subsetSetter = function(el, control) {
if (typeof control.subscenes === "undefined" ||
control.subscenes === null)
control.subscenes = this.scene.rootSubscene;
var value = Math.round(control.value),
subscenes = [].concat(control.subscenes),
fullset = [].concat(control.fullset),
i, j, entries, subsceneid, 
adds = [], deletes = [],
ismissing = function(x) {
return fullset.indexOf(x) < 0;
},
tointeger = function(x) {
return parseInt(x, 10);
};
if (control.accumulate)
for (i=0; i <= value; i++)
adds = adds.concat(control.subsets[i]);
else
adds = adds.concat(control.subsets[value]);
deletes = fullset.filter(function(x) { return adds.indexOf(x) < 0 });  
for (i = 0; i < subscenes.length; i++) {
subsceneid = subscenes[i];
if (typeof this.getObj(subsceneid) === "undefined")
this.alertOnce("typeof object is undefined");
for (j = 0; j < adds.length; j++)
this.addToSubscene(adds[j], subsceneid);
for (j = 0; j < deletes.length; j++)
this.delFromSubscene(deletes[j], subsceneid);
}
};
this.propertySetter = function(el, control)  {
var value = control.value,
values = [].concat(control.values),
svals = [].concat(control.param),
direct = values[0] === null,
entries = [].concat(control.entries),
ncol = entries.length,
nrow = values.length/ncol,
properties = this.repeatToLen(control.properties, ncol),
objids = this.repeatToLen(control.objids, ncol),
property, objid = objids[0],
obj = this.getObj(objid),
propvals, i, v1, v2, p, entry, gl, needsBinding,
newprop, newid,
getPropvals = function() {
if (property === "userMatrix")
return obj.par3d.userMatrix.getAsArray();
else if (property === "scale" || property === "FOV" || property === "zoom")
return [].concat(obj.par3d[property]);
else
return [].concat(obj[property]);
};
putPropvals = function(newvals) {
if (newvals.length == 1)
newvals = newvals[0];
if (property === "userMatrix")
obj.par3d.userMatrix.load(newvals);
else if (property === "scale" || property === "FOV" || property === "zoom")
obj.par3d[property] = newvals;
else
obj[property] = newvals;
}
if (direct && typeof value === "undefined")
return;
if (control.interp) {
values = values.slice(0, ncol).concat(values).
concat(values.slice(ncol*(nrow-1), ncol*nrow));
svals = [-Infinity].concat(svals).concat(Infinity);
for (i = 1; i < svals.length; i++) {
if (value <= svals[i]) {
if (svals[i] === Infinity)
p = 1;
else
p = (svals[i] - value)/(svals[i] - svals[i-1]);
break;
}
}
} else if (!direct) {
value = Math.round(value);
}
for (j=0; j<entries.length; j++) {
entry = entries[j];
newprop = properties[j];
newid = objids[j];
if (newprop !== property || newid != objid) {
if (typeof property !== "undefined")
putPropvals(propvals);
property = newprop;
objid = newid;
obj = this.getObj(objid);
propvals = getPropvals();
}
if (control.interp) {
v1 = values[ncol*(i-1) + j];
v2 = values[ncol*i + j];
this.setElement(propvals, entry, p*v1 + (1-p)*v2);
} else if (!direct) {
this.setElement(propvals, entry, values[ncol*value + j]);
} else {
this.setElement(propvals, entry, value[j]);
}
}
putPropvals(propvals);
needsBinding = [];
for (j=0; j < entries.length; j++) {
if (properties[j] === "values" &&
needsBinding.indexOf(objids[j]) === -1) {
needsBinding.push(objids[j]);
}
}
for (j=0; j < needsBinding.length; j++) {
gl = this.gl || this.initGL();
obj = this.getObj(needsBinding[j]);
gl.bindBuffer(gl.ARRAY_BUFFER, obj.buf);
gl.bufferData(gl.ARRAY_BUFFER, obj.values, gl.STATIC_DRAW);
}
};
this.vertexSetter = function(el, control)  {
var svals = [].concat(control.param),
j, k, p, propvals, stride, ofs, obj,
attrib,
ofss    = {x:"vofs", y:"vofs", z:"vofs",
red:"cofs", green:"cofs", blue:"cofs",
alpha:"cofs", radii:"radofs",
nx:"nofs", ny:"nofs", nz:"nofs",
ox:"oofs", oy:"oofs", oz:"oofs",
ts:"tofs", tt:"tofs"},
pos     = {x:0, y:1, z:2,
red:0, green:1, blue:2,
alpha:3,radii:0,
nx:0, ny:1, nz:2,
ox:0, oy:1, oz:2,
ts:0, tt:1},
values = control.values,
direct = values === null,
ncol,
interp = control.interp,
vertices = [].concat(control.vertices),
attributes = [].concat(control.attributes),
value = control.value;
ncol = Math.max(vertices.length, attributes.length);
if (!ncol)
return;
vertices = this.repeatToLen(vertices, ncol);
attributes = this.repeatToLen(attributes, ncol);
if (direct)
interp = false;
/* JSON doesn't pass Infinity */
svals[0] = -Infinity;
svals[svals.length - 1] = Infinity;
for (j = 1; j < svals.length; j++) {
if (value <= svals[j]) {
if (interp) {
if (svals[j] === Infinity)
p = 1;
else
p = (svals[j] - value)/(svals[j] - svals[j-1]);
} else {
if (svals[j] - value > value - svals[j-1])
j = j - 1;
}
break;
}
}
obj = this.getObj(control.objid);
propvals = obj.values;
for (k=0; k<ncol; k++) {
attrib = attributes[k];
vertex = vertices[k];
ofs = obj.vOffsets[ofss[attrib]];
if (ofs < 0)
this.alertOnce("Attribute '"+attrib+"' not found in object "+control.objid);
else {
stride = obj.vOffsets.stride;
ofs = vertex*stride + ofs + pos[attrib];
if (direct) {
propvals[ofs] = value;
} else if (interp) {
propvals[ofs] = p*values[j-1][k] + (1-p)*values[j][k];
} else {
propvals[ofs] = values[j][k];
}
}
}
if (typeof obj.buf !== "undefined") {
var gl = this.gl || this.initGL();
gl.bindBuffer(gl.ARRAY_BUFFER, obj.buf);
gl.bufferData(gl.ARRAY_BUFFER, propvals, gl.STATIC_DRAW);
}
};
this.ageSetter = function(el, control) {
var objids = [].concat(control.objids),
nobjs = objids.length,
time = control.value,
births = [].concat(control.births),
ages = [].concat(control.ages),
steps = births.length,
j = Array(steps),
p = Array(steps),
i, k, age, j0, propvals, stride, ofs, objid, obj,
attrib, dim,
attribs = ["colors", "alpha", "radii", "vertices",
"normals", "origins", "texcoords",
"x", "y", "z",
"red", "green", "blue"],
ofss    = ["cofs", "cofs", "radofs", "vofs",
"nofs", "oofs", "tofs",
"vofs", "vofs", "vofs",
"cofs", "cofs", "cofs"],
dims    = [3,1,1,3,
3,2,2,
1,1,1,
1,1,1],
pos     = [0,3,0,0,
0,0,0,
0,1,2,
0,1,2];
/* Infinity doesn't make it through JSON */
ages[0] = -Infinity;
ages[ages.length-1] = Infinity;
for (i = 0; i < steps; i++) {
if (births[i] !== null) {  // NA in R becomes null
age = time - births[i];
for (j0 = 1; age > ages[j0]; j0++);
if (ages[j0] == Infinity)
p[i] = 1;
else if (ages[j0] > ages[j0-1])
p[i] = (ages[j0] - age)/(ages[j0] - ages[j0-1]);
else
p[i] = 0;
j[i] = j0;
}
}
for (l = 0; l < nobjs; l++) {
objid = objids[l];
obj = this.getObj(objid);
if (typeof obj.vOffsets === "undefined")
continue;
propvals = obj.values;
stride = obj.vOffsets.stride;
for (k = 0; k < attribs.length; k++) {
attrib = control[attribs[k]];
if (typeof attrib !== "undefined") {
ofs = obj.vOffsets[ofss[k]];
if (ofs >= 0) {
dim = dims[k];
ofs = ofs + pos[k];
for (i = 0; i < steps; i++) {
if (births[i] !== null) {
for (d=0; d < dim; d++) {
propvals[i*stride + ofs + d] = p[i]*attrib[dim*(j[i]-1) + d] + (1-p[i])*attrib[dim*j[i] + d];
}
}
}
} else
this.alertOnce("\'"+attribs[k]+"\' property not found in object "+objid);
}
}
obj.values = propvals;
if (typeof obj.buf !== "undefined") {
gl = this.gl || this.initGL();
gl.bindBuffer(gl.ARRAY_BUFFER, obj.buf);
gl.bufferData(gl.ARRAY_BUFFER, obj.values, gl.STATIC_DRAW);
}
}
};
this.oldBridge = function(el, control) {
var attrname, global = window[control.prefix + "rgl"];
if (typeof global !== "undefined")
for (attrname in global)
this[attrname] = global[attrname];
window[control.prefix + "rgl"] = this;
};
this.Player = function(el, control) {
var
self = this,
components = [].concat(control.components),
buttonLabels = [].concat(control.buttonLabels),
Tick = function() { /* "this" will be a timer */
var i,
nominal = this.value,
slider = this.Slider,
labels = this.outputLabels,
output = this.Output,
step;
if (typeof slider !== "undefined" && nominal != slider.value)
slider.value = nominal;
if (typeof output !== "undefined") {
step = Math.round((nominal - output.sliderMin)/output.sliderStep);
if (labels !== null) {
output.innerHTML = labels[step];
} else {
step = step*output.sliderStep + output.sliderMin;
output.innerHTML = step.toPrecision(output.outputPrecision);
}
}
for (i=0; i < this.actions.length; i++) {
this.actions[i].value = nominal;
}
self.applyControls(el, this.actions, false);
self.drawScene();
},
OnSliderInput = function() { /* "this" will be the slider */
this.rgltimer.value = Number(this.value);
this.rgltimer.Tick();
},
addSlider = function(min, max, step, value) {
var slider = document.createElement("input");
slider.type = "range";
slider.min = min;
slider.max = max;
slider.step = step;
slider.value = value;
slider.oninput = OnSliderInput;
slider.sliderActions = control.actions;
slider.sliderScene = this;
slider.className = "rgl-slider";
slider.id = el.id + "-slider";
el.rgltimer.Slider = slider;
slider.rgltimer = el.rgltimer;
el.appendChild(slider);
},
addLabel = function(labels, min, step, precision) {
var output = document.createElement("output");
output.sliderMin = min;
output.sliderStep = step;
output.outputPrecision = precision;
output.className = "rgl-label";
output.id = el.id + "-label";
el.rgltimer.Output = output;
el.rgltimer.outputLabels = labels;
el.appendChild(output);
},
addButton = function(which, label, active) {
var button = document.createElement("input"),
onclicks = {Reverse: function() { this.rgltimer.reverse();},
Play: function() { this.rgltimer.play();
this.value = this.rgltimer.enabled ? this.inactiveValue : this.activeValue; },
Slower: function() { this.rgltimer.slower(); },
Faster: function() { this.rgltimer.faster(); },
Reset: function() { this.rgltimer.reset(); },
Step:  function() { this.rgltimer.step(); }
};
button.rgltimer = el.rgltimer;
button.type = "button";
button.value = label;
button.activeValue = label;
button.inactiveValue = active;
if (which === "Play")
button.rgltimer.PlayButton = button;
button.onclick = onclicks[which];
button.className = "rgl-button";
button.id = el.id + "-" + which;
el.appendChild(button);
};
if (typeof control.reinit !== "undefined" && control.reinit !== null) {
control.actions.reinit = control.reinit;
}
el.rgltimer = new rgltimerClass(Tick, control.start, control.interval, control.stop, 
control.step, control.value, control.rate, control.loop, control.actions);
for (var i=0; i < components.length; i++) {
switch(components[i]) {
case "Slider": addSlider(control.start, control.stop,
control.step, control.value);
break;
case "Label": addLabel(control.labels, control.start,
control.step, control.precision);
break;
default:
addButton(components[i], buttonLabels[i], control.pause);
}
}
el.rgltimer.Tick();
};
this.applyControls = function(el, x, draw) {
var self = this, reinit = x.reinit, i, control, type;
for (i = 0; i < x.length; i++) {
control = x[i];
type = control.type;
self[type](el, control);
}
if (typeof reinit !== "undefined" && reinit !== null) {
reinit = [].concat(reinit);
for (i = 0; i < reinit.length; i++)
self.getObj(reinit[i]).initialized = false;
}
if (typeof draw === "undefined" || draw)
self.drawScene();
};
this.sceneChangeHandler = function(message) {
var self = document.getElementById(message.elementId).rglinstance,
objs = message.objects, mat = message.material,
root = message.rootSubscene,
initSubs = message.initSubscenes,
redraw = message.redrawScene,
skipRedraw = message.skipRedraw,
deletes, subs, allsubs = [], i,j;
if (typeof message.delete !== "undefined") {
deletes = [].concat(message.delete);
if (typeof message.delfromSubscenes !== "undefined")
subs = [].concat(message.delfromSubscenes);
else
subs = [];
for (i = 0; i < deletes.length; i++) {
for (j = 0; j < subs.length; j++) {
self.delFromSubscene(deletes[i], subs[j]);
}
delete self.scene.objects[deletes[i]];
}
}
if (typeof objs !== "undefined") {
Object.keys(objs).forEach(function(key){
key = parseInt(key, 10);
self.scene.objects[key] = objs[key];
self.initObj(key);
var obj = self.getObj(key),
subs = [].concat(obj.inSubscenes), k;
allsubs = allsubs.concat(subs);
for (k = 0; k < subs.length; k++)
self.addToSubscene(key, subs[k]);
});
}
if (typeof mat !== "undefined") {
self.scene.material = mat;
}
if (typeof root !== "undefined") {
self.scene.rootSubscene = root;
}
if (typeof initSubs !== "undefined")
allsubs = allsubs.concat(initSubs);
allsubs = self.unique(allsubs);
for (i = 0; i < allsubs.length; i++) {
self.initSubscene(allsubs[i]);
}
if (typeof skipRedraw !== "undefined") {
root = self.getObj(self.scene.rootSubscene);
root.par3d.skipRedraw = skipRedraw;
}
if (redraw)
self.drawScene();
};
}).call(rglwidgetClass.prototype);
rgltimerClass = function(Tick, startTime, interval, stopTime, stepSize, value, rate, loop, actions) {
this.enabled = false;
this.timerId = 0;
this.startTime = startTime;         /* nominal start time in seconds */
this.value = value;                 /* current nominal time */
this.interval = interval;           /* seconds between updates */
this.stopTime = stopTime;           /* nominal stop time */
this.stepSize = stepSize;           /* nominal step size */
this.rate = rate;                   /* nominal units per second */
this.loop = loop;                   /* "none", "cycle", or "oscillate" */
this.realStart = undefined;         /* real world start time */
this.multiplier = 1;                /* multiplier for fast-forward
or reverse */
this.actions = actions;
this.Tick = Tick;
};
(function() {
this.play = function() {
if (this.enabled) {
this.enabled = false;
window.clearInterval(this.timerId);
this.timerId = 0;
return;
}
var tick = function(self) {
var now = new Date();
self.value = self.multiplier*self.rate*(now - self.realStart)/1000 + self.startTime;
self.forceToRange();
if (typeof self.Tick !== "undefined") {
self.Tick(self.value);
}
};
this.realStart = new Date() - 1000*(this.value - this.startTime)/this.rate/this.multiplier;
this.timerId = window.setInterval(tick, 1000*this.interval, this);
this.enabled = true;
};
this.forceToRange = function() {
if (this.value > this.stopTime + this.stepSize/2 || this.value < this.startTime - this.stepSize/2) {
if (!this.loop) {
this.reset();
} else {
var cycle = this.stopTime - this.startTime + this.stepSize,
newval = (this.value - this.startTime) % cycle + this.startTime;
if (newval < this.startTime) {
newval += cycle;
}
this.realStart += (this.value - newval)*1000/this.multiplier/this.rate;
this.value = newval;
}
}   
}
this.reset = function() {
this.value = this.startTime;
this.newmultiplier(1);
if (typeof this.Tick !== "undefined") {
this.Tick(this.value);
}
if (this.enabled)
this.play();  /* really pause... */
if (typeof this.PlayButton !== "undefined")
this.PlayButton.value = "Play";
};
this.faster = function() {
this.newmultiplier(Math.SQRT2*this.multiplier);
};
this.slower = function() {
this.newmultiplier(this.multiplier/Math.SQRT2);
};
this.reverse = function() {
this.newmultiplier(-this.multiplier);
};
this.newmultiplier = function(newmult) {
if (newmult != this.multiplier) {
this.realStart += 1000*(this.value - this.startTime)/this.rate*(1/this.multiplier - 1/newmult);
this.multiplier = newmult;
}
};
this.step = function() {
this.value += this.rate*this.multiplier;
this.forceToRange();
if (typeof this.Tick !== "undefined")
this.Tick(this.value);
}
}).call(rgltimerClass.prototype);</script>

<script type="text/javascript">
var make_webgl_plotdiv = document.getElementById("make_webgl_plotdiv"),
make_webgl_plotrgl = new rglwidgetClass();
make_webgl_plotdiv.width = 673;
make_webgl_plotdiv.height = 481;
make_webgl_plotrgl.initialize(make_webgl_plotdiv,
{"material":{"color":"#000000","alpha":1,"lit":true,"ambient":"#000000","specular":"#FFFFFF","emission":"#000000","shininess":50,"smooth":true,"front":"filled","back":"filled","size":3,"lwd":1,"fog":false,"point_antialias":false,"line_antialias":false,"texture":null,"textype":"rgb","texmipmap":false,"texminfilter":"linear","texmagfilter":"linear","texenvmap":false,"depth_mask":true,"depth_test":"less","isTransparent":false},"rootSubscene":1,"objects":{"7":{"id":7,"type":"points","material":{"lit":false},"vertices":[[-2.698091,-0.4080409,-1.76912],[-2.637779,-1.114133,-2.764644],[-2.631406,0.9478134,-2.266892],[-2.571282,0.4062715,-1.935792],[-2.520205,-2.007415,-0.2092482],[-2.49408,-1.15267,-0.6916792],[-2.488214,-0.454124,-2.327535],[-2.431154,0.5620464,-2.212973],[-2.279375,0.9095301,-1.258855],[-2.218185,0.8407594,-0.743318],[-2.206429,1.69437,-0.8344697],[-2.130762,0.2289791,-1.174041],[-2.125048,-1.236815,-3.305041],[-2.115046,1.36806,-0.1179394],[-2.100243,0.3919316,-0.1762427],[-2.07752,0.8435321,-1.432019],[-2.061161,-0.2003821,-1.952392],[-2.030655,0.1826273,-1.165502],[-2.022753,-0.3048269,-1.982972],[-2.005112,-0.4695318,-2.338596],[-1.997121,0.5552371,-0.7160828],[-1.962802,1.671005,-2.11938],[-1.956655,-0.389133,-3.699353],[-1.916015,-1.301409,-3.498494],[-1.863129,-0.5784134,-0.9827686],[-1.815447,-1.417874,-3.880229],[-1.813529,-0.3897848,-2.255027],[-1.811164,0.9880743,-0.4142622],[-1.805886,-1.742736,-1.658464],[-1.8029,1.324158,0.2519142],[-1.779614,-0.3091734,0.2186788],[-1.756822,-1.079334,-0.4388887],[-1.7378,-0.04048441,-0.5894462],[-1.698555,0.3416635,-1.697803],[-1.685394,1.468867,-0.9085705],[-1.681077,-2.311589,-2.135552],[-1.670356,-1.041247,-2.421996],[-1.666382,0.04949416,-2.595493],[-1.650653,0.2667539,-0.9970327],[-1.642211,0.007295691,-0.8015478],[-1.637707,0.6304171,-0.8641736],[-1.636384,0.9194499,-0.9028106],[-1.628015,0.3380439,-2.273988],[-1.624335,-1.050829,-2.267389],[-1.621563,1.14028,-1.406858],[-1.592113,-2.258818,-3.757425],[-1.557702,-0.4102478,-1.450684],[-1.548336,1.627731,-1.798639],[-1.54402,-0.4665647,-2.78739],[-1.52978,-1.291124,-1.442681],[-1.517074,1.118201,-1.113821],[-1.50925,0.4629269,0.4298691],[-1.5078,-0.3025064,-2.331395],[-1.488861,1.713611,-0.3755476],[-1.483055,-0.8805195,-0.6597895],[-1.471262,1.339724,-0.1739275],[-1.464529,-1.39897,-1.280993],[-1.461643,-1.46631,-3.487593],[-1.45946,-0.05830733,-2.81902],[-1.448041,0.2291663,-3.009178],[-1.446364,0.7843973,-1.319198],[-1.439575,-0.1453528,-3.104674],[-1.428142,1.480904,0.5984457],[-1.415928,-0.5270498,-0.948473],[-1.415109,1.073227,-2.547175],[-1.411382,-0.04508885,-1.773215],[-1.402826,1.142594,-1.351002],[-1.401057,-0.04655431,-0.72347],[-1.387316,0.3452573,-2.080706],[-1.381935,-1.560418,-0.9185835],[-1.373541,-0.1314221,-0.08847083],[-1.370575,2.067857,-0.287512],[-1.369273,-0.8930652,0.02985884],[-1.367462,1.512091,-0.5018241],[-1.349328,-0.3775712,-1.880146],[-1.34633,0.292183,-0.6123413],[-1.330106,-0.6147658,-3.395294],[-1.317121,0.724816,0.3045906],[-1.310972,-0.5056617,-3.342377],[-1.308796,-0.1353574,-3.12704],[-1.270029,0.9719494,-1.143129],[-1.264143,2.275384,-1.589488],[-1.2587,0.3231464,-2.30558],[-1.252624,-0.4447128,-0.8737275],[-1.243932,-0.7066392,-2.497147],[-1.243228,-0.2813906,-3.538901],[-1.237578,-2.181597,-3.566605],[-1.233066,1.653888,-1.710227],[-1.193051,-0.844381,-3.026067],[-1.190462,0.9669572,-1.470503],[-1.185031,0.4402163,-0.5088989],[-1.180425,0.7722347,-0.664161],[-1.180298,-0.7605204,-3.281185],[-1.173418,-1.086407,-0.3120941],[-1.16337,0.8124267,0.2462392],[-1.161243,-0.1282299,-3.766619],[-1.161177,-3.373272,-2.587497],[-1.160781,-1.937922,-1.497985],[-1.158506,-0.3211184,-1.951529],[-1.157862,-0.05703938,-0.4337835],[-1.143217,0.2255575,-0.5616106],[-1.143123,-0.447091,-0.9887124],[-1.139763,0.3682939,-1.996024],[-1.129897,-1.501088,-0.6557833],[-1.12388,-0.4682844,-4.0435],[-1.122168,-0.03295878,-1.630967],[-1.119234,-0.1074243,-0.07264001],[-1.118077,0.1334426,-1.806964],[-1.10721,-1.1483,-2.346153],[-1.105984,1.71596,-0.03061939],[-1.104531,-0.4978469,-1.153467],[-1.098507,-0.697894,-1.970042],[-1.097518,-1.991783,-2.179152],[-1.096223,1.592031,0.6123182],[-1.086095,-0.842384,-1.489882],[-1.082095,-0.1822308,-1.706206],[-1.06495,0.8619565,-1.438262],[-1.06061,-0.2620655,-1.748204],[-1.049157,0.6114593,-1.598771],[-1.039289,1.248997,-1.136132],[-1.036985,0.0200948,-0.1126768],[-1.033325,0.7954077,-0.03734311],[-1.028365,-0.9911182,-2.31225],[-1.027873,0.3923371,-0.5575404],[-1.025892,0.08114433,-1.813293],[-1.019427,-2.10445,-2.24143],[-1.015807,-1.004918,-3.219041],[-1.012472,0.2611223,-2.932039],[-1.011829,0.4337084,-2.612563],[-1.009016,2.178419,-0.85855],[-1.005545,1.025889,-0.8478997],[-1.005375,-0.5867808,-3.14396],[-1.004193,-1.433446,-4.077253],[-1.002042,-0.4859253,-0.9872164],[-0.9810383,-0.6501265,-1.822258],[-0.9798455,0.4308822,-2.060099],[-0.9789453,0.7486247,-1.257631],[-0.9769762,-1.313802,-3.294516],[-0.9735089,-0.3614621,-1.11302],[-0.9662206,2.130858,-1.78092],[-0.9608331,-0.1856087,-0.1610271],[-0.957248,1.009095,-3.014186],[-0.9522665,1.581114,-1.345273],[-0.9518015,-0.3178067,-1.418682],[-0.9511651,0.361706,-0.3702571],[-0.9493716,-0.03418205,-0.5940401],[-0.9454431,1.045498,-0.3392339],[-0.9414859,0.3879681,0.8728906],[-0.9366803,0.07056493,-0.6595999],[-0.935514,-0.6702787,-1.9789],[-0.9319037,0.4326209,-1.333417],[-0.9305358,1.035042,-0.3103826],[-0.9198744,-0.5414942,-2.390758],[-0.9194896,-0.07301731,-2.570297],[-0.9171023,1.682066,-0.1003172],[-0.9167874,1.069854,-0.9524096],[-0.9132155,-2.154278,-3.15969],[-0.9099437,0.8864462,-1.876207],[-0.9095918,-0.3562178,-1.135869],[-0.9065561,-1.294125,-2.785586],[-0.9063889,-1.213645,-1.960377],[-0.9058974,0.3764872,-2.600366],[-0.9051586,-1.202225,-3.177583],[-0.9020471,0.7040314,-3.000552],[-0.8908447,0.2778449,-1.529278],[-0.8882083,-2.041682,-3.441813],[-0.8881252,-1.159925,-3.360448],[-0.8838823,-1.202148,-3.824492],[-0.8747594,0.2988319,-1.459606],[-0.8738883,0.205947,-3.003015],[-0.8556798,-1.129688,-2.314965],[-0.8525199,-1.489681,-2.666042],[-0.8499789,-1.143675,-3.769358],[-0.8473707,0.4213509,0.200822],[-0.8451714,-0.6983646,-3.996256],[-0.835476,1.769531,-0.5116935],[-0.8195153,0.1656586,-2.499632],[-0.8188432,1.037585,-0.4478517],[-0.8165238,-1.258911,-2.609368],[-0.8119736,1.710424,0.3156086],[-0.8099792,-0.3742204,-1.790658],[-0.806048,2.363864,-1.722336],[-0.7896665,1.952395,1.671042],[-0.7823162,0.9081082,-1.06177],[-0.7820765,-1.164374,-2.589573],[-0.7790845,1.050596,-2.277492],[-0.7742296,0.06950713,-2.856788],[-0.7694524,-0.158636,-0.107083],[-0.7688795,2.195357,-0.08027815],[-0.7674901,0.5120604,-1.483922],[-0.7670977,0.5232366,-1.891829],[-0.7663085,0.9065226,-1.779113],[-0.7648789,0.12569,-2.587086],[-0.7604551,0.3619495,-1.407441],[-0.7604001,0.2363012,-0.9518843],[-0.7572739,0.7022168,-0.3559606],[-0.7546893,0.8385613,0.5923507],[-0.7393116,-1.029072,-1.486246],[-0.7370597,0.469158,-2.081596],[-0.734809,1.09493,-2.06259],[-0.7317324,-1.581961,-3.259968],[-0.7303221,-1.189968,-2.40447],[-0.7295313,0.04897291,-2.02366],[-0.7288415,0.6366655,-2.372563],[-0.7241259,-0.5264788,-3.277179],[-0.7228695,-0.5824551,-2.371223],[-0.7221947,0.2328071,-0.7156168],[-0.7184792,-2.711999,-4.731017],[-0.7139469,-0.9905515,-4.055326],[-0.7137544,0.3031659,-2.240981],[-0.7047544,-2.249536,-2.458701],[-0.7036772,0.1401746,-2.835915],[-0.6996373,-0.08898135,-1.193529],[-0.6894154,0.9054654,0.7917381],[-0.684857,-1.557857,-3.718032],[-0.6799761,0.4174405,0.06496211],[-0.6789712,0.599727,-1.240965],[-0.6774897,-1.402166,-4.588302],[-0.6770941,1.100089,0.3318267],[-0.6733527,0.5200213,-1.688498],[-0.6669374,1.628231,0.618764],[-0.6653032,1.272414,-0.001394736],[-0.6650122,-0.2865419,-1.081018],[-0.6576598,0.1932764,-1.52726],[-0.6566704,-0.6818507,-3.436878],[-0.654785,0.331051,-2.573356],[-0.653459,0.893591,0.1418726],[-0.6519669,-0.2263845,-2.523035],[-0.63763,0.3361015,-1.357811],[-0.6358801,0.3039913,-1.654265],[-0.6350691,-1.075634,-2.835335],[-0.634765,1.28776,0.7867885],[-0.6346654,0.7851076,-1.785953],[-0.6215844,-0.005072016,0.2192618],[-0.6203451,-1.019973,-2.098602],[-0.6200355,-0.8753759,-1.946508],[-0.6190248,-0.9561642,-2.75547],[-0.6158292,0.1411151,-1.921741],[-0.6156764,-0.8062926,-3.168903],[-0.6116183,1.028607,-0.5838537],[-0.6109166,0.8861306,-1.668317],[-0.6051939,-0.8399727,-4.110571],[-0.6033479,0.2029544,-0.9148265],[-0.6030278,0.7187622,-1.840425],[-0.5969848,-0.0763858,-3.786528],[-0.5934302,-0.01876669,-1.939486],[-0.5916129,0.549907,1.039174],[-0.5906686,-0.5276714,-3.170129],[-0.5895686,0.6571725,-0.4440079],[-0.5825979,-0.9070962,-2.705044],[-0.5777975,-1.93892,-2.54493],[-0.5713635,1.904418,-1.134574],[-0.5687147,-0.5536809,-2.577267],[-0.56762,0.1261611,0.2739055],[-0.5673387,0.2190202,-0.4730177],[-0.5635263,-0.8209667,-2.696876],[-0.5633803,0.6667966,-1.178007],[-0.5609791,-0.01147309,-3.718887],[-0.5566204,-1.121715,-3.239694],[-0.5547042,1.393735,1.373659],[-0.5506601,0.5574716,-0.1992434],[-0.5488752,-0.3848815,-0.9283347],[-0.5465384,0.336027,-1.493575],[-0.5424608,-0.7203184,-3.097332],[-0.5419819,0.1633648,-2.284838],[-0.537316,0.6292602,-2.197813],[-0.5314407,-0.1063861,-2.041021],[-0.5258936,1.554844,-0.3244994],[-0.518261,0.1668947,0.5646821],[-0.5168147,0.8651507,-1.586255],[-0.5124969,1.289725,-0.9342691],[-0.5093526,-1.002429,-3.384512],[-0.5081859,-0.3444803,-2.534862],[-0.5042306,-0.3676987,-3.424247],[-0.4991435,-1.29814,-3.256097],[-0.4938933,2.13863,-1.543794],[-0.4905628,-0.105808,-4.153374],[-0.4632948,-0.08956581,0.3671083],[-0.462242,-0.4452449,-3.678756],[-0.4598178,0.3123131,-1.563636],[-0.4579085,-1.171562,-3.050532],[-0.4571829,-1.155661,-2.62979],[-0.4555885,0.158503,-0.5967985],[-0.4505356,-0.2214887,-3.033619],[-0.4492,0.3477039,-2.788363],[-0.4451596,-0.6371142,-3.747141],[-0.4355633,0.7588331,0.3953794],[-0.4324713,1.822245,-0.3152229],[-0.4323308,0.9348637,0.003525726],[-0.430856,-2.10856,-2.340064],[-0.427603,0.7273439,-2.013025],[-0.427059,0.562022,0.9273976],[-0.4258322,-0.2263272,-2.398669],[-0.4255326,-0.7420785,-3.59415],[-0.4235487,-0.1536957,-1.700931],[-0.422997,-1.71553,-4.244068],[-0.4219474,-0.3947441,-1.503824],[-0.4199802,0.5638148,-1.877793],[-0.4198428,-0.7637995,-0.779233],[-0.4195411,0.7284507,0.9946264],[-0.4182427,0.7524051,1.309018],[-0.4130744,-0.5071849,-4.646426],[-0.4127306,-2.444218,-4.276469],[-0.4074863,1.740883,2.186598],[-0.4047172,-0.8157475,-2.592817],[-0.4042428,-0.6007112,-4.541303],[-0.4014494,-1.184638,-1.752691],[-0.401058,-0.1039305,-2.826417],[-0.4000622,1.063493,-1.470216],[-0.3996611,1.270032,0.003053421],[-0.3949125,0.6023921,0.60997],[-0.3904801,0.3549418,-1.148826],[-0.3901789,0.07323388,-0.4345543],[-0.3883133,0.6924286,-1.161256],[-0.3856604,0.7247332,-1.056151],[-0.3832497,-0.5446194,-1.550043],[-0.382416,2.515012,0.504063],[-0.3799231,0.1048764,-0.8248582],[-0.3736042,-2.381332,-1.602795],[-0.3722229,-0.6873915,-3.813433],[-0.3720607,1.467341,-0.09685422],[-0.3718465,0.004949955,-1.563512],[-0.3651185,1.497823,0.4107747],[-0.3632361,-0.7783043,-3.561366],[-0.362766,0.892673,-1.622595],[-0.3623515,0.07167102,-1.45435],[-0.3607741,0.271972,-1.752027],[-0.357387,1.654218,-0.8755962],[-0.3567552,-0.8000213,-3.316426],[-0.3547385,0.4989192,1.536792],[-0.3543888,0.9127979,0.2646095],[-0.3531546,0.5102568,-1.289154],[-0.3521383,1.574702,-0.2693968],[-0.3503703,-0.2655068,0.6225836],[-0.3477166,0.355396,1.151731],[-0.3461865,0.05690519,-2.289649],[-0.3396887,-0.7372035,-2.4137],[-0.3361985,-0.4073493,-2.09083],[-0.3322604,0.8130036,-0.3763643],[-0.3314615,0.6417056,-1.493562],[-0.3284412,0.573934,-0.01537053],[-0.3232373,0.3427506,-1.004992],[-0.3223144,-0.2264019,-1.800611],[-0.3220647,-1.4741,-1.575236],[-0.321702,0.6495224,-2.247921],[-0.3183335,-0.6583936,-5.111271],[-0.3168585,-1.138842,-4.789872],[-0.3130007,-0.3643673,-0.9059695],[-0.3080356,-1.261544,-2.445725],[-0.3078932,1.586707,0.8083549],[-0.3056579,2.735496,-1.314825],[-0.3007271,-1.496468,-3.315562],[-0.2988934,-0.4212792,-3.110469],[-0.2982858,1.092083,-2.092518],[-0.2953873,-1.59215,-2.87607],[-0.2953284,0.7905375,-0.0243004],[-0.2949135,0.936312,-0.6058944],[-0.2946482,-0.3392917,-4.119346],[-0.2914252,0.004957126,-3.652316],[-0.2861176,-0.7237874,-1.809586],[-0.2848259,-0.2593644,-4.637676],[-0.2785428,1.536434,-1.20908],[-0.2738813,0.3800033,0.1972911],[-0.266385,-0.8320367,-3.1875],[-0.2659507,0.371642,-3.00505],[-0.2657536,-1.142177,-2.34039],[-0.265693,0.8472182,0.9760464],[-0.2644366,1.691763,0.3982249],[-0.2644193,-0.8897371,-2.298346],[-0.2638619,-0.196309,-2.211884],[-0.2566052,0.466148,-0.9603851],[-0.2563379,-1.356335,-3.56431],[-0.2506005,-0.6312032,-3.611964],[-0.2491361,0.009636628,-1.127292],[-0.2464486,0.1022438,0.4969673],[-0.2439465,0.2984191,-0.4964958],[-0.2432354,2.217471,-0.3699724],[-0.240802,-0.2436822,-0.5246931],[-0.2372501,0.5421916,-1.817028],[-0.2372,0.1252587,-0.7952527],[-0.236009,0.97574,0.5739176],[-0.230422,0.7911978,-1.455905],[-0.2249817,-0.2890697,-3.044905],[-0.2233801,0.7753204,1.365036],[-0.2163164,-0.4230817,-5.502378],[-0.2140484,-1.243644,-3.485529],[-0.2116904,0.6959137,-0.3855352],[-0.2104816,0.07800603,-1.404323],[-0.2083904,0.3824568,0.1978458],[-0.2081065,-0.8628402,-2.072748],[-0.2044751,0.3017265,0.6082758],[-0.2026758,-1.075858,-1.981921],[-0.2008818,-0.8588784,-3.504908],[-0.2003343,0.6284487,0.248236],[-0.1950007,-0.0216812,-2.747977],[-0.1946933,0.7250155,-0.4237607],[-0.1943946,-0.944368,-4.211428],[-0.1930731,-0.4321227,-5.023059],[-0.1918401,0.2303864,-0.2319509],[-0.1846236,-1.221525,-1.953704],[-0.1709775,-1.24737,-2.288052],[-0.1698286,-0.09805026,-1.929898],[-0.1688243,-1.759035,-5.309244],[-0.1635815,-0.539978,-2.726539],[-0.1631199,-1.580539,-2.335995],[-0.1628721,1.310174,0.2468637],[-0.1623996,0.6034477,-0.3268369],[-0.1622692,-1.554209,-1.668662],[-0.1573042,0.2524717,-0.7656935],[-0.1557268,1.30069,1.879949],[-0.1557143,-0.7917352,-3.837595],[-0.1542536,0.8903826,-0.1487134],[-0.1496972,0.336609,-0.3723722],[-0.1435716,-1.261334,-2.708295],[-0.1432522,-1.936337,-3.666569],[-0.1390259,-0.7926073,-3.196707],[-0.1365294,-0.3529029,-3.888285],[-0.1358165,-0.3647227,-2.994819],[-0.1298983,1.965306,-0.4707854],[-0.1295084,1.794977,0.4262459],[-0.1217616,0.1149236,-1.59439],[-0.1193313,0.04491994,-0.7857456],[-0.1161639,1.44568,-1.690723],[-0.1077462,-0.2918859,-3.310585],[-0.1073995,0.1874958,0.9589948],[-0.1039017,-1.088678,-2.907706],[-0.1028396,1.536511,-0.0220005],[-0.1018052,0.01033358,-3.495059],[-0.09813314,0.8721092,-1.628011],[-0.09474822,1.722009,-0.5528703],[-0.09353525,-0.4729759,-2.207963],[-0.09261701,-0.1344131,-2.393118],[-0.09225906,0.2349937,-0.8614672],[-0.09051056,0.7477192,-1.083562],[-0.08122503,-0.3832135,-3.08911],[-0.08078411,0.2170746,-0.8727877],[-0.08017219,-1.848542,-2.707301],[-0.07588977,-0.7098573,-2.021685],[-0.07508266,-0.2019708,-2.57279],[-0.07265466,1.039457,0.6435497],[-0.07062629,-0.635135,-3.248814],[-0.07060023,-0.9374727,-4.931874],[-0.06870703,-0.9685352,-3.443696],[-0.06622264,-0.3668982,-0.658648],[-0.06565817,-1.493756,-3.532003],[-0.0634596,-1.094989,-2.883778],[-0.06286602,-0.3359954,-4.175107],[-0.06099125,-0.007517906,-1.523343],[-0.05996166,-0.5161564,-3.175692],[-0.05893727,0.2022601,-0.1632594],[-0.05818862,-0.2356097,-2.134159],[-0.0581149,-0.9975072,-4.219564],[-0.05707483,-0.2393312,-2.832616],[-0.05441905,0.1034779,-0.7086146],[-0.04316797,0.9889518,-0.344826],[-0.04024342,-0.8354899,-1.017092],[-0.03971476,1.114836,-0.8503198],[-0.03684003,-0.4247808,-1.956688],[-0.0273591,1.743472,-0.2624162],[-0.01645292,-1.238911,-2.655582],[-0.01010572,0.9220865,-0.6761742],[-0.008838071,1.326341,0.7621205],[-0.002313605,-0.068172,-2.710202],[0.001812595,0.7596155,-0.4834485],[0.003432657,1.077457,-0.7613002],[0.006883266,0.3561597,-1.895805],[0.007084948,1.470047,0.7047943],[0.008568361,1.0811,-0.6778371],[0.009386154,-1.590224,5.278797],[0.009499406,0.4922326,-0.693838],[0.01279531,0.3792539,-0.317825],[0.01293129,0.867364,-0.1079426],[0.02005621,0.610877,0.5612233],[0.02053929,-1.046772,3.686784],[0.02118182,0.4431384,0.1003768],[0.02205424,-0.8308192,3.218258],[0.02508244,1.346163,-0.5618241],[0.02847881,0.4516073,0.3055841],[0.03318108,1.284448,-2.517566],[0.03626271,-0.1929761,1.750634],[0.03951141,-1.559149,0.1389875],[0.04015632,0.8378853,1.104267],[0.04150062,-0.02785678,0.9155779],[0.04345989,-0.0005061282,-0.4774049],[0.04368677,0.6140209,-0.1840782],[0.04672507,-2.488326,3.194548],[0.04713364,0.9070284,-0.1178593],[0.048007,1.66789,0.7496345],[0.04950205,0.8418329,1.964226],[0.05759111,-0.9299825,3.548196],[0.05878518,0.2620185,-0.6564575],[0.06085861,0.9298986,1.734678],[0.06206696,-0.6784609,4.802381],[0.06774449,0.120316,1.692232],[0.06834574,-0.6603213,1.514189],[0.07345307,0.07259899,1.984072],[0.07904813,-0.4019794,1.804169],[0.07965027,1.013108,-0.05824713],[0.08401249,-0.6157677,3.305694],[0.08423501,1.870788,1.045576],[0.0848085,2.151655,-0.5486975],[0.08588469,0.3899944,1.422788],[0.08661131,0.3580842,-0.7143657],[0.0976164,1.125269,-1.100317],[0.09818124,0.001045209,1.314199],[0.1015329,0.1811291,0.474447],[0.1040705,0.2480165,0.6953393],[0.1041191,0.6186721,1.24741],[0.1051646,1.091265,-0.5246451],[0.1070454,0.9589002,0.3248403],[0.1116562,0.01632002,0.7462374],[0.1146098,-0.862379,2.993078],[0.1192865,0.9114322,0.07197124],[0.1197646,1.041851,-0.1843769],[0.1200723,-0.7677283,2.184489],[0.12073,-1.098498,2.988836],[0.1211344,-0.1036005,1.076609],[0.1215862,-0.09105407,2.000539],[0.1219391,1.665641,0.5219961],[0.1221875,1.688687,1.084745],[0.1251151,-2.635203,1.392912],[0.1284442,1.523833,-0.8597882],[0.1288203,0.817822,2.094497],[0.1343286,1.532055,-2.208463],[0.1359837,1.817955,-0.2682519],[0.1369409,1.218032,-1.929489],[0.1395478,-1.452,1.323458],[0.1398607,-0.332212,3.153262],[0.1462861,-0.964784,3.198637],[0.1518212,-0.1172259,1.638678],[0.1521644,0.2425358,1.816544],[0.1528199,-1.10007,2.440952],[0.1531148,-0.8698508,3.535704],[0.1569578,0.1990724,1.200775],[0.1582122,-1.066121,3.177363],[0.1588119,0.5911132,4.292241],[0.1621042,1.173005,1.196853],[0.1684612,-2.164667,3.321435],[0.1689034,-0.6567931,3.097414],[0.1715889,-0.6481953,3.802719],[0.1724113,0.3602505,-0.3235134],[0.1740109,-0.2393473,2.614601],[0.1756197,0.3411574,0.1105718],[0.1791185,1.263699,-0.1977402],[0.181284,0.5729772,1.659784],[0.1827165,-0.2600631,2.264797],[0.1885678,-0.1984457,2.8651],[0.1913959,-0.1591747,2.292788],[0.1940335,-1.670159,3.843458],[0.1953471,0.5709532,0.5800666],[0.1957086,-1.410069,2.49492],[0.1957262,-1.075089,1.931562],[0.1986117,-0.09004831,0.1758073],[0.1988815,0.5485587,0.6020435],[0.1991411,-0.1701113,2.829118],[0.2016805,0.4048135,0.1384804],[0.202189,1.242675,-0.6309758],[0.2031734,-0.4375891,2.998678],[0.2069724,1.215751,-0.9091671],[0.2139657,-0.5416396,2.360194],[0.2147508,-1.12056,2.553622],[0.2151123,-0.5117682,1.932628],[0.2162486,-0.3566471,4.714458],[0.2200797,-0.6716508,2.581077],[0.2268337,1.009351,2.071296],[0.228423,0.4660832,0.8225064],[0.2336136,0.1925828,2.515578],[0.2368285,-1.121868,1.635603],[0.2384231,0.7309346,-0.2894985],[0.2430142,-1.243578,3.106453],[0.244416,-0.4948615,3.223708],[0.24616,0.04172675,3.174353],[0.2466138,1.275117,0.5600567],[0.2505524,0.1334458,-0.2358251],[0.2508502,-1.079516,2.467839],[0.2570375,-1.341715,2.400166],[0.2598632,-0.3499724,3.007662],[0.2603341,0.7869673,0.1063933],[0.2658474,-1.2702,1.880655],[0.2697341,0.4466847,2.015799],[0.2704147,-0.4370944,3.301746],[0.2705155,0.09332839,1.175375],[0.2706131,1.838022,0.5245526],[0.2735542,-0.2049775,0.6447899],[0.27507,0.5275751,0.9391739],[0.276589,2.60855,-0.07501526],[0.2768284,-0.3354309,1.074475],[0.2795765,0.02891534,2.112596],[0.2860261,-0.5305522,2.665777],[0.2895666,0.04406894,2.646468],[0.2901129,-1.090124,1.727443],[0.2905112,-2.729313,3.443655],[0.2927979,-0.3876833,1.828834],[0.2929574,-0.1695271,4.052578],[0.2959261,-0.6607987,4.277666],[0.3011252,-0.3880153,1.958698],[0.3037549,-0.9752401,2.384552],[0.3050085,1.571636,0.11497],[0.3065719,-0.197489,1.024735],[0.30798,-0.7643813,2.584569],[0.309844,2.438415,-0.3420215],[0.3102541,-0.3404696,2.237585],[0.3102771,0.7934153,0.5106215],[0.3176392,0.2501701,0.186722],[0.320444,-0.3450103,3.083241],[0.3268745,-2.343349,2.90884],[0.3313397,-0.00528594,2.573796],[0.3332015,-0.4876401,1.952486],[0.3335301,0.8295106,-0.367959],[0.3352174,-0.1699234,2.970796],[0.3367765,-0.1780721,3.601411],[0.3367987,0.01659263,1.413836],[0.337074,-0.1465353,1.666172],[0.3408413,-1.947814,2.931629],[0.3423882,-0.3350493,1.6535],[0.3425475,-0.08980317,2.976043],[0.3447686,-0.09343401,0.65075],[0.3460817,0.92146,0.1411331],[0.3468923,0.2515321,3.135735],[0.3485621,-0.002988767,1.647349],[0.3544721,-0.6961325,1.392889],[0.3582797,1.324971,1.356916],[0.361293,-0.6787231,1.38406],[0.377406,-0.8295141,3.612067],[0.3774472,0.5461547,1.832012],[0.3802534,0.5473651,0.9398766],[0.3822131,0.09418357,1.835489],[0.3846381,-0.7070186,2.38037],[0.3857914,0.5253182,2.895688],[0.3864283,0.100635,-1.303319],[0.3883243,1.085542,1.30739],[0.3884434,-1.361676,4.219064],[0.3906546,0.4204496,0.2067005],[0.3926565,1.453383,0.3631559],[0.3959146,0.1180114,2.168771],[0.3966169,-0.01097088,1.780837],[0.3998742,0.3105161,1.327788],[0.4023733,1.061697,0.172362],[0.4024663,1.488258,0.2825376],[0.4049831,-0.1372966,1.450333],[0.4070815,1.623314,-0.1359093],[0.4072874,0.3574727,0.2821209],[0.4080309,-0.2023629,3.125954],[0.4087716,0.2616907,0.5092579],[0.4107708,0.484103,0.5652008],[0.4214772,-1.253253,2.433156],[0.4258258,1.732374,0.122647],[0.4271102,-0.3167876,2.584437],[0.4291143,-1.033394,2.411768],[0.4302541,1.685133,-1.18015],[0.430499,-1.1281,2.352909],[0.4306156,0.01386302,0.3007972],[0.4339869,-0.3189112,3.410777],[0.4358712,0.5250663,0.7003641],[0.437061,0.3250695,1.742687],[0.4380359,-0.4482374,2.332215],[0.4382087,1.112584,-0.1449097],[0.4391219,-0.1945085,3.03302],[0.4418333,0.07780182,1.867019],[0.4462044,-1.082819,1.769722],[0.4539274,-1.265427,2.674355],[0.4587994,2.826212,-0.2826629],[0.4661267,-0.6896868,1.637072],[0.4762367,-1.927014,4.772276],[0.4796323,0.1583006,0.003201153],[0.4914576,-0.5350713,3.541009],[0.4941262,1.555298,0.506023],[0.4985195,-0.9990884,3.208999],[0.5035469,0.00292291,0.9636858],[0.5036188,-1.076429,1.476316],[0.5037387,0.3403272,-0.8795208],[0.5040942,-0.6331748,3.138385],[0.5041246,0.6873325,-0.2538242],[0.5108652,-0.1971188,0.3919107],[0.5123335,0.9464762,-1.276282],[0.514573,-1.100777,2.041081],[0.5232197,-0.4754086,0.2669801],[0.5246616,0.5519691,1.592524],[0.5249542,1.882982,-1.29034],[0.5257537,0.121223,-0.1132431],[0.527084,1.184118,1.054984],[0.5288602,0.7104708,-0.4720231],[0.5292203,-1.174892,3.573759],[0.5293068,0.562215,-0.9993421],[0.529401,-0.1792114,2.176404],[0.5303524,-0.7702365,2.494879],[0.5331351,0.2659586,1.108809],[0.5357813,0.170885,-0.03434777],[0.5399267,-0.3927397,2.791984],[0.5475723,-2.428782,4.045996],[0.5506817,-1.534902,3.81695],[0.5509267,0.9349398,1.241572],[0.5526863,-0.2205319,0.6217749],[0.5528677,0.5277748,0.002925963],[0.5541881,0.3400356,2.220291],[0.5572597,0.9761556,0.5705109],[0.5609733,-0.3878081,3.166619],[0.5634833,-1.392774,2.424805],[0.5649506,-0.8097829,3.26117],[0.564993,0.669199,0.6019622],[0.5658673,-0.3291692,1.762526],[0.5701275,1.343877,0.4681846],[0.5706323,1.413484,0.8607],[0.5747957,-2.378414,3.17378],[0.5748736,2.118655,0.6636286],[0.5806504,1.766709,-0.8654594],[0.5957807,1.46292,0.1307891],[0.5977429,0.3664061,0.6620684],[0.6001395,-0.939175,2.460921],[0.6020398,-0.4710108,2.199513],[0.6072915,-0.7289116,2.971318],[0.6087049,-1.460716,2.975296],[0.6114322,-0.03350366,1.026004],[0.6117378,-0.7124367,2.140786],[0.6131028,0.04041529,1.648116],[0.6131378,-0.8553314,2.388083],[0.6138982,0.3109937,1.212934],[0.6143004,-0.3814468,0.5457022],[0.6150035,-0.02043513,0.05830723],[0.619427,-0.8325016,2.08549],[0.6207013,1.530461,-0.02719495],[0.623992,1.25185,-0.6681079],[0.6272979,0.2603117,0.5937074],[0.629252,-1.788529,3.727866],[0.6295785,2.263831,0.02995208],[0.631432,0.4893525,1.034901],[0.6324325,1.690956,-1.385897],[0.633716,-0.322964,1.611322],[0.6341376,-2.189028,2.874702],[0.6389514,-1.460427,1.406355],[0.640519,0.1133094,1.174263],[0.6465757,-1.033824,3.617109],[0.6504012,-0.132866,1.490862],[0.6506715,-0.07762062,0.684756],[0.6519278,0.83666,0.5362657],[0.6581618,-0.01902647,0.5112641],[0.6590939,1.174151,0.4029754],[0.6605209,-0.7730879,2.129319],[0.6620032,-0.927947,1.796711],[0.6652226,-0.4260406,2.989936],[0.6667013,-1.122087,2.09269],[0.6682823,-0.7570854,2.299217],[0.6685271,-1.735452,3.469269],[0.6708747,0.2973047,0.8147609],[0.6725983,0.5361043,0.2595777],[0.679687,-0.6284421,2.691635],[0.682505,1.911495,0.2539294],[0.689503,-0.3533106,1.735549],[0.6914628,-0.4657383,2.184181],[0.6945168,0.05506413,1.076715],[0.694911,-1.544917,2.248113],[0.698279,0.5849785,2.457001],[0.7007616,0.5318586,1.034361],[0.7040047,-0.4243233,2.24655],[0.7085949,0.1537888,2.054559],[0.7094249,-0.8849318,3.779192],[0.7115123,0.2259416,0.2595224],[0.7145249,0.4282376,0.9161701],[0.7174255,0.6872815,0.4225381],[0.7185168,0.263021,0.864014],[0.7261225,-0.9197111,1.095416],[0.731901,0.8647686,-0.501723],[0.7319885,-0.522621,0.9036686],[0.732135,1.380175,1.848474],[0.7367971,-1.696914,3.017326],[0.7402681,0.784453,0.2868021],[0.7490141,0.2157636,2.32671],[0.7498145,0.5384114,1.664881],[0.7515372,0.5584492,0.4332291],[0.7583815,0.884971,0.6900428],[0.7589937,0.06125825,1.622093],[0.7652376,-0.7173911,2.127562],[0.7707299,1.218393,-0.004672097],[0.7728253,-0.8522486,3.499798],[0.7739784,0.7527077,0.5252245],[0.7785763,-1.174398,2.499739],[0.7824852,0.05252993,3.488452],[0.7828597,-2.121817,1.851346],[0.7829956,-1.10839,2.350967],[0.7859297,-0.05734969,2.270116],[0.787077,-1.21577,3.681608],[0.7874984,1.919383,1.056439],[0.7897992,1.236324,0.5262888],[0.7936214,-1.370202,2.379059],[0.79371,0.5438022,0.983529],[0.8037671,-1.938315,4.682467],[0.8053132,0.8168406,0.9626284],[0.8086342,-0.4711715,1.508469],[0.8168249,-0.7385294,2.987521],[0.8187673,-0.05200801,2.177418],[0.8191684,0.1302,0.8359406],[0.820609,0.8463227,-0.3790026],[0.8210493,-0.09368182,2.934045],[0.8236374,-1.039212,3.812617],[0.8365081,-0.3142788,1.168845],[0.8378435,-1.877236,2.04246],[0.8388656,0.03910939,3.054918],[0.8392184,-0.278073,2.362769],[0.8429994,0.08140069,1.448758],[0.8449441,-0.7162265,3.49184],[0.8471092,0.3277474,1.399607],[0.8478581,-1.356214,3.826423],[0.8480466,-0.0006776184,1.980172],[0.8493193,-1.729952,4.762691],[0.8517476,0.9358752,0.3762315],[0.8536209,0.1083648,1.286754],[0.8551846,0.5611799,0.2664768],[0.8600633,0.9236026,-0.7545143],[0.8607278,-0.5300078,3.889536],[0.8709834,0.9155363,1.353194],[0.8790511,-1.261532,3.621005],[0.8794314,0.8833605,1.670024],[0.8827633,-0.264115,1.4215],[0.8836516,1.527928,0.07822686],[0.8901792,1.743508,0.06968565],[0.8902851,1.179011,-0.3770482],[0.898315,-1.341383,0.8318654],[0.9052142,0.1577815,1.537428],[0.9251912,0.5696683,0.01032451],[0.9263664,-0.3258111,3.037855],[0.9291855,0.8950209,1.087441],[0.9399319,-0.01522179,1.92121],[0.9417318,-1.074945,2.52898],[0.9456897,0.3196077,0.6187835],[0.9472497,-0.2953741,0.5150495],[0.9510632,-1.319862,0.4516211],[0.9590859,-0.04199061,3.322856],[0.9608966,1.72986,0.5333407],[0.9645988,0.01078481,1.032337],[0.9706286,-0.02768979,2.226699],[0.9745019,2.15598,-0.1951266],[0.9790424,-0.1943412,3.194515],[0.9798916,-0.1350506,0.3051732],[1.001038,0.1922232,2.31716],[1.003506,-0.6672732,0.3559859],[1.005136,1.310082,-0.2724518],[1.00604,1.235212,0.1636746],[1.009257,0.818991,0.798971],[1.009898,-0.09889916,1.489801],[1.010851,1.461258,0.830278],[1.012594,0.7260576,0.3863858],[1.013735,-2.024965,2.741685],[1.0148,1.867101,0.4778246],[1.026486,0.444443,1.148251],[1.032578,3.929453,0.6772913],[1.038344,0.4556497,0.9920177],[1.043534,-1.182491,3.355423],[1.049209,-1.48341,3.486662],[1.049764,0.0001563914,2.843569],[1.050058,-2.565974,3.398218],[1.051728,-0.1914604,2.457246],[1.065887,0.5080581,-0.03352394],[1.083362,-0.5346677,3.389852],[1.08544,0.6371027,0.5802428],[1.091124,1.619805,1.019797],[1.098061,-0.8401135,3.310541],[1.108472,0.5857105,1.633426],[1.112721,-1.658852,2.895774],[1.116165,-0.8754113,1.216229],[1.123223,1.010192,-0.09042454],[1.123498,1.189096,2.144435],[1.131314,1.844496,-1.067471],[1.137105,-2.108459,0.4211389],[1.14248,0.1386632,2.490434],[1.149183,1.168466,0.06934381],[1.152966,-0.4978215,3.400487],[1.155059,-0.3947623,0.5271118],[1.157787,-1.961428,1.769382],[1.16182,0.5152891,1.385135],[1.169896,-2.110233,3.756084],[1.170793,0.3439417,1.505593],[1.170854,-0.8733917,2.256283],[1.171709,-0.4323568,2.323275],[1.175518,-0.0129686,2.943067],[1.180016,0.1841998,1.143342],[1.180432,0.0960728,2.041894],[1.184007,0.02907228,2.63708],[1.193166,-1.317384,2.945866],[1.19443,-0.2070305,0.4541642],[1.196899,0.7174106,1.346149],[1.200341,1.080394,1.401444],[1.209206,0.710912,0.4147272],[1.209287,-0.2682787,2.661009],[1.217167,-0.3099277,3.092848],[1.227294,0.08106135,2.370942],[1.227368,-0.560647,1.269849],[1.232824,-0.6741372,2.33677],[1.234102,-0.07629745,0.7653582],[1.237428,-0.4043078,1.000211],[1.240998,0.8320726,1.122382],[1.241493,-0.07657186,2.559841],[1.245285,-1.853403,2.861397],[1.25076,0.7453831,0.5228604],[1.252245,-0.1682813,3.063012],[1.25373,1.008792,-0.6239855],[1.25886,0.09677546,2.147908],[1.264579,0.4327875,3.699487],[1.270713,-0.4295955,0.850283],[1.28631,-0.1717996,1.329924],[1.291406,-0.8607772,1.923424],[1.29263,-0.7982724,2.794434],[1.296311,0.2883966,2.673794],[1.301249,-0.1600685,0.6552262],[1.311234,-1.091398,1.978164],[1.314118,-1.575289,3.569396],[1.316715,0.7825914,1.631115],[1.33678,0.08512583,1.530362],[1.339389,1.424475,0.1552918],[1.340501,1.257537,-0.08586977],[1.346658,0.4381994,2.690944],[1.34814,2.13171,1.368518],[1.35,0.3796011,0.8665748],[1.354288,-0.410195,0.6183834],[1.365495,-0.3579386,0.2867709],[1.36778,-0.7031512,1.516487],[1.370116,-0.5280742,0.8111314],[1.376507,-0.1288136,2.337526],[1.381366,1.749755,0.1173815],[1.383291,1.711621,0.03486221],[1.389437,-0.9601376,2.209979],[1.390774,1.492439,1.071004],[1.39924,0.392227,-0.4341336],[1.408319,0.1224214,2.170386],[1.41212,-0.117387,-0.2661201],[1.420146,-0.1808504,1.56945],[1.420246,0.9336858,-0.217022],[1.428311,-0.9010071,3.063232],[1.430947,0.08613319,2.061031],[1.434135,0.3500811,1.829485],[1.450235,0.02845597,3.360028],[1.456256,0.01605218,0.8965483],[1.457709,-1.466699,3.77387],[1.461437,1.000209,0.2904622],[1.464231,0.4363409,2.921478],[1.465059,1.707238,0.06218401],[1.465343,2.316457,1.079146],[1.467998,-0.7935233,2.101578],[1.485586,-0.009334872,2.775597],[1.489327,0.0731075,-0.8102003],[1.49157,1.280403,0.8012057],[1.491677,0.2259324,1.848422],[1.51439,0.2834936,2.849153],[1.533839,0.03451316,1.808234],[1.536489,1.35683,0.0939148],[1.53868,-0.3772756,0.8359445],[1.542996,0.4326922,1.219797],[1.546921,0.8864947,2.361369],[1.558613,-0.3036741,3.857315],[1.595239,0.5506546,2.048374],[1.614082,0.04368478,-0.006941631],[1.619569,1.240772,-0.3618338],[1.621916,2.045002,3.8168],[1.655582,-0.09464794,1.917448],[1.65742,-1.007649,1.934678],[1.660047,0.09451885,2.396542],[1.685994,-1.088573,0.4929277],[1.699416,-1.31965,3.762499],[1.703572,1.42724,1.21735],[1.724865,0.4607578,2.679675],[1.751509,1.215007,2.816626],[1.762631,-1.852116,1.242268],[1.763343,-0.5073424,3.049484],[1.77172,-0.5221603,0.98738],[1.810705,0.2762617,0.03110171],[1.811237,0.08419959,0.9628761],[1.816308,-1.669909,3.036588],[1.829711,-0.1963245,0.7179686],[1.832075,0.1642444,2.065993],[1.841522,0.8321584,2.145656],[1.849665,-0.04614455,2.810349],[1.852056,0.8573142,-0.4961838],[1.862955,0.652155,-0.7340718],[1.870465,-0.3404808,3.981937],[1.888949,1.36021,1.59528],[1.907029,0.5373367,1.763463],[1.919762,-0.2815144,2.289186],[1.929926,0.3805474,1.577436],[1.937357,0.3677595,-0.866981],[1.952521,-0.7366079,3.457722],[1.970138,2.339844,-0.4397327],[1.976849,0.02831855,0.8026706],[1.984188,0.02518677,3.347633],[1.998785,0.3870761,0.9632369],[2.018219,0.6992081,0.7087033],[2.052705,-0.1838838,2.874257],[2.122437,-0.6115611,1.718803],[2.141348,0.01007263,2.219761],[2.142747,0.06405229,0.9034356],[2.177831,-0.08335208,0.2058818],[2.216152,0.2915374,2.429371],[2.272345,-2.408828,3.979109],[2.298579,-1.603539,1.262664],[2.304026,1.31907,-0.9915661],[2.419471,-0.6271333,0.4266405],[2.45774,-0.5470855,2.114122],[2.475149,2.321609,-0.8786085],[2.480019,0.04906114,1.297351],[2.607543,-0.2347621,0.008657314],[2.707211,-0.5342721,2.258714],[2.723716,0.729841,1.430411]],"colors":[[1,0,0,1],[1,0.007843138,0,1],[1,0.01176471,0,1],[1,0.01960784,0,1],[1,0.02352941,0,1],[1,0.03137255,0,1],[1,0.03529412,0,1],[1,0.04313726,0,1],[1,0.04705882,0,1],[1,0.05490196,0,1],[1,0.05882353,0,1],[1,0.06666667,0,1],[1,0.07058824,0,1],[1,0.07843138,0,1],[1,0.08235294,0,1],[1,0.09019608,0,1],[1,0.09411765,0,1],[1,0.1019608,0,1],[1,0.1098039,0,1],[1,0.1137255,0,1],[1,0.1215686,0,1],[1,0.1254902,0,1],[1,0.1333333,0,1],[1,0.1372549,0,1],[1,0.145098,0,1],[1,0.1490196,0,1],[1,0.1568628,0,1],[1,0.1607843,0,1],[1,0.1686275,0,1],[1,0.172549,0,1],[1,0.1803922,0,1],[1,0.1843137,0,1],[1,0.1921569,0,1],[1,0.1960784,0,1],[1,0.2039216,0,1],[1,0.2117647,0,1],[1,0.2156863,0,1],[1,0.2235294,0,1],[1,0.227451,0,1],[1,0.2352941,0,1],[1,0.2392157,0,1],[1,0.2470588,0,1],[1,0.2509804,0,1],[1,0.2588235,0,1],[1,0.2627451,0,1],[1,0.2705882,0,1],[1,0.2745098,0,1],[1,0.282353,0,1],[1,0.2862745,0,1],[1,0.2941177,0,1],[1,0.3019608,0,1],[1,0.3058824,0,1],[1,0.3137255,0,1],[1,0.3176471,0,1],[1,0.3254902,0,1],[1,0.3294118,0,1],[1,0.3372549,0,1],[1,0.3411765,0,1],[1,0.3490196,0,1],[1,0.3529412,0,1],[1,0.3607843,0,1],[1,0.3647059,0,1],[1,0.372549,0,1],[1,0.3764706,0,1],[1,0.3843137,0,1],[1,0.3882353,0,1],[1,0.3960784,0,1],[1,0.4039216,0,1],[1,0.4078431,0,1],[1,0.4156863,0,1],[1,0.4196078,0,1],[1,0.427451,0,1],[1,0.4313726,0,1],[1,0.4392157,0,1],[1,0.4431373,0,1],[1,0.4509804,0,1],[1,0.454902,0,1],[1,0.4627451,0,1],[1,0.4666667,0,1],[1,0.4745098,0,1],[1,0.4784314,0,1],[1,0.4862745,0,1],[1,0.4901961,0,1],[1,0.4980392,0,1],[1,0.5058824,0,1],[1,0.509804,0,1],[1,0.5176471,0,1],[1,0.5215687,0,1],[1,0.5294118,0,1],[1,0.5333334,0,1],[1,0.5411765,0,1],[1,0.5450981,0,1],[1,0.5529412,0,1],[1,0.5568628,0,1],[1,0.5647059,0,1],[1,0.5686275,0,1],[1,0.5764706,0,1],[1,0.5803922,0,1],[1,0.5882353,0,1],[1,0.5921569,0,1],[1,0.6,0,1],[1,0.6078432,0,1],[1,0.6117647,0,1],[1,0.6196079,0,1],[1,0.6235294,0,1],[1,0.6313726,0,1],[1,0.6352941,0,1],[1,0.6431373,0,1],[1,0.6470588,0,1],[1,0.654902,0,1],[1,0.6588235,0,1],[1,0.6666667,0,1],[1,0.6705883,0,1],[1,0.6784314,0,1],[1,0.682353,0,1],[1,0.6901961,0,1],[1,0.6941177,0,1],[1,0.7019608,0,1],[1,0.7098039,0,1],[1,0.7137255,0,1],[1,0.7215686,0,1],[1,0.7254902,0,1],[1,0.7333333,0,1],[1,0.7372549,0,1],[1,0.7450981,0,1],[1,0.7490196,0,1],[1,0.7568628,0,1],[1,0.7607843,0,1],[1,0.7686275,0,1],[1,0.772549,0,1],[1,0.7803922,0,1],[1,0.7843137,0,1],[1,0.7921569,0,1],[1,0.7960784,0,1],[1,0.8039216,0,1],[1,0.8117647,0,1],[1,0.8156863,0,1],[1,0.8235294,0,1],[1,0.827451,0,1],[1,0.8352941,0,1],[1,0.8392157,0,1],[1,0.8470588,0,1],[1,0.8509804,0,1],[1,0.8588235,0,1],[1,0.8627451,0,1],[1,0.8705882,0,1],[1,0.8745098,0,1],[1,0.8823529,0,1],[1,0.8862745,0,1],[1,0.8941177,0,1],[1,0.8980392,0,1],[1,0.9058824,0,1],[1,0.9137255,0,1],[1,0.9176471,0,1],[1,0.9254902,0,1],[1,0.9294118,0,1],[1,0.9372549,0,1],[1,0.9411765,0,1],[1,0.9490196,0,1],[1,0.9529412,0,1],[1,0.9607843,0,1],[1,0.9647059,0,1],[1,0.972549,0,1],[1,0.9764706,0,1],[1,0.9843137,0,1],[1,0.9882353,0,1],[1,0.9960784,0,1],[0.9960784,1,0,1],[0.9921569,1,0,1],[0.9843137,1,0,1],[0.9803922,1,0,1],[0.972549,1,0,1],[0.9686275,1,0,1],[0.9607843,1,0,1],[0.9568627,1,0,1],[0.9490196,1,0,1],[0.945098,1,0,1],[0.9372549,1,0,1],[0.9333333,1,0,1],[0.9254902,1,0,1],[0.9215686,1,0,1],[0.9137255,1,0,1],[0.9098039,1,0,1],[0.9019608,1,0,1],[0.8941177,1,0,1],[0.8901961,1,0,1],[0.8823529,1,0,1],[0.8784314,1,0,1],[0.8705882,1,0,1],[0.8666667,1,0,1],[0.8588235,1,0,1],[0.854902,1,0,1],[0.8470588,1,0,1],[0.8431373,1,0,1],[0.8352941,1,0,1],[0.8313726,1,0,1],[0.8235294,1,0,1],[0.8196079,1,0,1],[0.8117647,1,0,1],[0.8078431,1,0,1],[0.8,1,0,1],[0.7921569,1,0,1],[0.7882353,1,0,1],[0.7803922,1,0,1],[0.7764706,1,0,1],[0.7686275,1,0,1],[0.7647059,1,0,1],[0.7568628,1,0,1],[0.7529412,1,0,1],[0.7450981,1,0,1],[0.7411765,1,0,1],[0.7333333,1,0,1],[0.7294118,1,0,1],[0.7215686,1,0,1],[0.7176471,1,0,1],[0.7098039,1,0,1],[0.7058824,1,0,1],[0.6980392,1,0,1],[0.6901961,1,0,1],[0.6862745,1,0,1],[0.6784314,1,0,1],[0.6745098,1,0,1],[0.6666667,1,0,1],[0.6627451,1,0,1],[0.654902,1,0,1],[0.6509804,1,0,1],[0.6431373,1,0,1],[0.6392157,1,0,1],[0.6313726,1,0,1],[0.627451,1,0,1],[0.6196079,1,0,1],[0.6156863,1,0,1],[0.6078432,1,0,1],[0.6039216,1,0,1],[0.5960785,1,0,1],[0.5882353,1,0,1],[0.5843138,1,0,1],[0.5764706,1,0,1],[0.572549,1,0,1],[0.5647059,1,0,1],[0.5607843,1,0,1],[0.5529412,1,0,1],[0.5490196,1,0,1],[0.5411765,1,0,1],[0.5372549,1,0,1],[0.5294118,1,0,1],[0.5254902,1,0,1],[0.5176471,1,0,1],[0.5137255,1,0,1],[0.5058824,1,0,1],[0.5019608,1,0,1],[0.4941176,1,0,1],[0.4862745,1,0,1],[0.4823529,1,0,1],[0.4745098,1,0,1],[0.4705882,1,0,1],[0.4627451,1,0,1],[0.4588235,1,0,1],[0.4509804,1,0,1],[0.4470588,1,0,1],[0.4392157,1,0,1],[0.4352941,1,0,1],[0.427451,1,0,1],[0.4235294,1,0,1],[0.4156863,1,0,1],[0.4117647,1,0,1],[0.4039216,1,0,1],[0.3960784,1,0,1],[0.3921569,1,0,1],[0.3843137,1,0,1],[0.3803922,1,0,1],[0.372549,1,0,1],[0.3686275,1,0,1],[0.3607843,1,0,1],[0.3568628,1,0,1],[0.3490196,1,0,1],[0.345098,1,0,1],[0.3372549,1,0,1],[0.3333333,1,0,1],[0.3254902,1,0,1],[0.3215686,1,0,1],[0.3137255,1,0,1],[0.3098039,1,0,1],[0.3019608,1,0,1],[0.2941177,1,0,1],[0.2901961,1,0,1],[0.282353,1,0,1],[0.2784314,1,0,1],[0.2705882,1,0,1],[0.2666667,1,0,1],[0.2588235,1,0,1],[0.254902,1,0,1],[0.2470588,1,0,1],[0.2431373,1,0,1],[0.2352941,1,0,1],[0.2313726,1,0,1],[0.2235294,1,0,1],[0.2196078,1,0,1],[0.2117647,1,0,1],[0.2078431,1,0,1],[0.2,1,0,1],[0.1921569,1,0,1],[0.1882353,1,0,1],[0.1803922,1,0,1],[0.1764706,1,0,1],[0.1686275,1,0,1],[0.1647059,1,0,1],[0.1568628,1,0,1],[0.1529412,1,0,1],[0.145098,1,0,1],[0.1411765,1,0,1],[0.1333333,1,0,1],[0.1294118,1,0,1],[0.1215686,1,0,1],[0.1176471,1,0,1],[0.1098039,1,0,1],[0.1058824,1,0,1],[0.09803922,1,0,1],[0.09019608,1,0,1],[0.08627451,1,0,1],[0.07843138,1,0,1],[0.07450981,1,0,1],[0.06666667,1,0,1],[0.0627451,1,0,1],[0.05490196,1,0,1],[0.05098039,1,0,1],[0.04313726,1,0,1],[0.03921569,1,0,1],[0.03137255,1,0,1],[0.02745098,1,0,1],[0.01960784,1,0,1],[0.01568628,1,0,1],[0.007843138,1,0,1],[0.003921569,1,0,1],[0,1,0.003921569,1],[0,1,0.01176471,1],[0,1,0.01568628,1],[0,1,0.02352941,1],[0,1,0.02745098,1],[0,1,0.03529412,1],[0,1,0.03921569,1],[0,1,0.04705882,1],[0,1,0.05098039,1],[0,1,0.05882353,1],[0,1,0.0627451,1],[0,1,0.07058824,1],[0,1,0.07450981,1],[0,1,0.08235294,1],[0,1,0.08627451,1],[0,1,0.09411765,1],[0,1,0.1019608,1],[0,1,0.1058824,1],[0,1,0.1137255,1],[0,1,0.1176471,1],[0,1,0.1254902,1],[0,1,0.1294118,1],[0,1,0.1372549,1],[0,1,0.1411765,1],[0,1,0.1490196,1],[0,1,0.1529412,1],[0,1,0.1607843,1],[0,1,0.1647059,1],[0,1,0.172549,1],[0,1,0.1764706,1],[0,1,0.1843137,1],[0,1,0.1882353,1],[0,1,0.1960784,1],[0,1,0.2039216,1],[0,1,0.2078431,1],[0,1,0.2156863,1],[0,1,0.2196078,1],[0,1,0.227451,1],[0,1,0.2313726,1],[0,1,0.2392157,1],[0,1,0.2431373,1],[0,1,0.2509804,1],[0,1,0.254902,1],[0,1,0.2627451,1],[0,1,0.2666667,1],[0,1,0.2745098,1],[0,1,0.2784314,1],[0,1,0.2862745,1],[0,1,0.2901961,1],[0,1,0.2980392,1],[0,1,0.3058824,1],[0,1,0.3098039,1],[0,1,0.3176471,1],[0,1,0.3215686,1],[0,1,0.3294118,1],[0,1,0.3333333,1],[0,1,0.3411765,1],[0,1,0.345098,1],[0,1,0.3529412,1],[0,1,0.3568628,1],[0,1,0.3647059,1],[0,1,0.3686275,1],[0,1,0.3764706,1],[0,1,0.3803922,1],[0,1,0.3882353,1],[0,1,0.3921569,1],[0,1,0.4,1],[0,1,0.4078431,1],[0,1,0.4117647,1],[0,1,0.4196078,1],[0,1,0.4235294,1],[0,1,0.4313726,1],[0,1,0.4352941,1],[0,1,0.4431373,1],[0,1,0.4470588,1],[0,1,0.454902,1],[0,1,0.4588235,1],[0,1,0.4666667,1],[0,1,0.4705882,1],[0,1,0.4784314,1],[0,1,0.4823529,1],[0,1,0.4901961,1],[0,1,0.4941176,1],[0,1,0.5019608,1],[0,1,0.509804,1],[0,1,0.5137255,1],[0,1,0.5215687,1],[0,1,0.5254902,1],[0,1,0.5333334,1],[0,1,0.5372549,1],[0,1,0.5450981,1],[0,1,0.5490196,1],[0,1,0.5568628,1],[0,1,0.5607843,1],[0,1,0.5686275,1],[0,1,0.572549,1],[0,1,0.5803922,1],[0,1,0.5843138,1],[0,1,0.5921569,1],[0,1,0.5960785,1],[0,1,0.6039216,1],[0,1,0.6117647,1],[0,1,0.6156863,1],[0,1,0.6235294,1],[0,1,0.627451,1],[0,1,0.6352941,1],[0,1,0.6392157,1],[0,1,0.6470588,1],[0,1,0.6509804,1],[0,1,0.6588235,1],[0,1,0.6627451,1],[0,1,0.6705883,1],[0,1,0.6745098,1],[0,1,0.682353,1],[0,1,0.6862745,1],[0,1,0.6941177,1],[0,1,0.7019608,1],[0,1,0.7058824,1],[0,1,0.7137255,1],[0,1,0.7176471,1],[0,1,0.7254902,1],[0,1,0.7294118,1],[0,1,0.7372549,1],[0,1,0.7411765,1],[0,1,0.7490196,1],[0,1,0.7529412,1],[0,1,0.7607843,1],[0,1,0.7647059,1],[0,1,0.772549,1],[0,1,0.7764706,1],[0,1,0.7843137,1],[0,1,0.7882353,1],[0,1,0.7960784,1],[0,1,0.8039216,1],[0,1,0.8078431,1],[0,1,0.8156863,1],[0,1,0.8196079,1],[0,1,0.827451,1],[0,1,0.8313726,1],[0,1,0.8392157,1],[0,1,0.8431373,1],[0,1,0.8509804,1],[0,1,0.854902,1],[0,1,0.8627451,1],[0,1,0.8666667,1],[0,1,0.8745098,1],[0,1,0.8784314,1],[0,1,0.8862745,1],[0,1,0.8901961,1],[0,1,0.8980392,1],[0,1,0.9058824,1],[0,1,0.9098039,1],[0,1,0.9176471,1],[0,1,0.9215686,1],[0,1,0.9294118,1],[0,1,0.9333333,1],[0,1,0.9411765,1],[0,1,0.945098,1],[0,1,0.9529412,1],[0,1,0.9568627,1],[0,1,0.9647059,1],[0,1,0.9686275,1],[0,1,0.9764706,1],[0,1,0.9803922,1],[0,1,0.9882353,1],[0,1,0.9921569,1],[0,1,1,1],[0,0.9921569,1,1],[0,0.9882353,1,1],[0,0.9803922,1,1],[0,0.9764706,1,1],[0,0.9686275,1,1],[0,0.9647059,1,1],[0,0.9568627,1,1],[0,0.9529412,1,1],[0,0.945098,1,1],[0,0.9411765,1,1],[0,0.9333333,1,1],[0,0.9294118,1,1],[0,0.9215686,1,1],[0,0.9176471,1,1],[0,0.9098039,1,1],[0,0.9058824,1,1],[0,0.8980392,1,1],[0,0.8901961,1,1],[0,0.8862745,1,1],[0,0.8784314,1,1],[0,0.8745098,1,1],[0,0.8666667,1,1],[0,0.8627451,1,1],[0,0.854902,1,1],[0,0.8509804,1,1],[0,0.8431373,1,1],[0,0.8392157,1,1],[0,0.8313726,1,1],[0,0.827451,1,1],[0,0.8196079,1,1],[0,0.8156863,1,1],[0,0.8078431,1,1],[0,0.8039216,1,1],[0,0.7960784,1,1],[0,0.7882353,1,1],[0,0.7843137,1,1],[0,0.7764706,1,1],[0,0.772549,1,1],[0,0.7647059,1,1],[0,0.7607843,1,1],[0,0.7529412,1,1],[0,0.7490196,1,1],[0,0.7411765,1,1],[0,0.7372549,1,1],[0,0.7294118,1,1],[0,0.7254902,1,1],[0,0.7176471,1,1],[0,0.7137255,1,1],[0,0.7058824,1,1],[0,0.6980392,1,1],[0,0.6941177,1,1],[0,0.6862745,1,1],[0,0.682353,1,1],[0,0.6745098,1,1],[0,0.6705883,1,1],[0,0.6627451,1,1],[0,0.6588235,1,1],[0,0.6509804,1,1],[0,0.6470588,1,1],[0,0.6392157,1,1],[0,0.6352941,1,1],[0,0.627451,1,1],[0,0.6235294,1,1],[0,0.6156863,1,1],[0,0.6117647,1,1],[0,0.6039216,1,1],[0,0.5960785,1,1],[0,0.5921569,1,1],[0,0.5843138,1,1],[0,0.5803922,1,1],[0,0.572549,1,1],[0,0.5686275,1,1],[0,0.5607843,1,1],[0,0.5568628,1,1],[0,0.5490196,1,1],[0,0.5450981,1,1],[0,0.5372549,1,1],[0,0.5333334,1,1],[0,0.5254902,1,1],[0,0.5215687,1,1],[0,0.5137255,1,1],[0,0.509804,1,1],[0,0.5019608,1,1],[0,0.4941176,1,1],[0,0.4901961,1,1],[0,0.4823529,1,1],[0,0.4784314,1,1],[0,0.4705882,1,1],[0,0.4666667,1,1],[0,0.4588235,1,1],[0,0.454902,1,1],[0,0.4470588,1,1],[0,0.4431373,1,1],[0,0.4352941,1,1],[0,0.4313726,1,1],[0,0.4235294,1,1],[0,0.4196078,1,1],[0,0.4117647,1,1],[0,0.4078431,1,1],[0,0.4,1,1],[0,0.3921569,1,1],[0,0.3882353,1,1],[0,0.3803922,1,1],[0,0.3764706,1,1],[0,0.3686275,1,1],[0,0.3647059,1,1],[0,0.3568628,1,1],[0,0.3529412,1,1],[0,0.345098,1,1],[0,0.3411765,1,1],[0,0.3333333,1,1],[0,0.3294118,1,1],[0,0.3215686,1,1],[0,0.3176471,1,1],[0,0.3098039,1,1],[0,0.3058824,1,1],[0,0.2980392,1,1],[0,0.2901961,1,1],[0,0.2862745,1,1],[0,0.2784314,1,1],[0,0.2745098,1,1],[0,0.2666667,1,1],[0,0.2627451,1,1],[0,0.254902,1,1],[0,0.2509804,1,1],[0,0.2431373,1,1],[0,0.2392157,1,1],[0,0.2313726,1,1],[0,0.227451,1,1],[0,0.2196078,1,1],[0,0.2156863,1,1],[0,0.2078431,1,1],[0,0.2039216,1,1],[0,0.1960784,1,1],[0,0.1882353,1,1],[0,0.1843137,1,1],[0,0.1764706,1,1],[0,0.172549,1,1],[0,0.1647059,1,1],[0,0.1607843,1,1],[0,0.1529412,1,1],[0,0.1490196,1,1],[0,0.1411765,1,1],[0,0.1372549,1,1],[0,0.1294118,1,1],[0,0.1254902,1,1],[0,0.1176471,1,1],[0,0.1137255,1,1],[0,0.1058824,1,1],[0,0.09803922,1,1],[0,0.09411765,1,1],[0,0.08627451,1,1],[0,0.08235294,1,1],[0,0.07450981,1,1],[0,0.07058824,1,1],[0,0.0627451,1,1],[0,0.05882353,1,1],[0,0.05098039,1,1],[0,0.04705882,1,1],[0,0.03921569,1,1],[0,0.03529412,1,1],[0,0.02745098,1,1],[0,0.02352941,1,1],[0,0.01568628,1,1],[0,0.01176471,1,1],[0,0.003921569,1,1],[0.003921569,0,1,1],[0.007843138,0,1,1],[0.01568628,0,1,1],[0.01960784,0,1,1],[0.02745098,0,1,1],[0.03137255,0,1,1],[0.03921569,0,1,1],[0.04313726,0,1,1],[0.05098039,0,1,1],[0.05490196,0,1,1],[0.0627451,0,1,1],[0.06666667,0,1,1],[0.07450981,0,1,1],[0.07843138,0,1,1],[0.08627451,0,1,1],[0.09019608,0,1,1],[0.09803922,0,1,1],[0.1058824,0,1,1],[0.1098039,0,1,1],[0.1176471,0,1,1],[0.1215686,0,1,1],[0.1294118,0,1,1],[0.1333333,0,1,1],[0.1411765,0,1,1],[0.145098,0,1,1],[0.1529412,0,1,1],[0.1568628,0,1,1],[0.1647059,0,1,1],[0.1686275,0,1,1],[0.1764706,0,1,1],[0.1803922,0,1,1],[0.1882353,0,1,1],[0.1921569,0,1,1],[0.2,0,1,1],[0.2078431,0,1,1],[0.2117647,0,1,1],[0.2196078,0,1,1],[0.2235294,0,1,1],[0.2313726,0,1,1],[0.2352941,0,1,1],[0.2431373,0,1,1],[0.2470588,0,1,1],[0.254902,0,1,1],[0.2588235,0,1,1],[0.2666667,0,1,1],[0.2705882,0,1,1],[0.2784314,0,1,1],[0.282353,0,1,1],[0.2901961,0,1,1],[0.2941177,0,1,1],[0.3019608,0,1,1],[0.3098039,0,1,1],[0.3137255,0,1,1],[0.3215686,0,1,1],[0.3254902,0,1,1],[0.3333333,0,1,1],[0.3372549,0,1,1],[0.345098,0,1,1],[0.3490196,0,1,1],[0.3568628,0,1,1],[0.3607843,0,1,1],[0.3686275,0,1,1],[0.372549,0,1,1],[0.3803922,0,1,1],[0.3843137,0,1,1],[0.3921569,0,1,1],[0.3960784,0,1,1],[0.4039216,0,1,1],[0.4117647,0,1,1],[0.4156863,0,1,1],[0.4235294,0,1,1],[0.427451,0,1,1],[0.4352941,0,1,1],[0.4392157,0,1,1],[0.4470588,0,1,1],[0.4509804,0,1,1],[0.4588235,0,1,1],[0.4627451,0,1,1],[0.4705882,0,1,1],[0.4745098,0,1,1],[0.4823529,0,1,1],[0.4862745,0,1,1],[0.4941176,0,1,1],[0.5019608,0,1,1],[0.5058824,0,1,1],[0.5137255,0,1,1],[0.5176471,0,1,1],[0.5254902,0,1,1],[0.5294118,0,1,1],[0.5372549,0,1,1],[0.5411765,0,1,1],[0.5490196,0,1,1],[0.5529412,0,1,1],[0.5607843,0,1,1],[0.5647059,0,1,1],[0.572549,0,1,1],[0.5764706,0,1,1],[0.5843138,0,1,1],[0.5882353,0,1,1],[0.5960785,0,1,1],[0.6039216,0,1,1],[0.6078432,0,1,1],[0.6156863,0,1,1],[0.6196079,0,1,1],[0.627451,0,1,1],[0.6313726,0,1,1],[0.6392157,0,1,1],[0.6431373,0,1,1],[0.6509804,0,1,1],[0.654902,0,1,1],[0.6627451,0,1,1],[0.6666667,0,1,1],[0.6745098,0,1,1],[0.6784314,0,1,1],[0.6862745,0,1,1],[0.6901961,0,1,1],[0.6980392,0,1,1],[0.7058824,0,1,1],[0.7098039,0,1,1],[0.7176471,0,1,1],[0.7215686,0,1,1],[0.7294118,0,1,1],[0.7333333,0,1,1],[0.7411765,0,1,1],[0.7450981,0,1,1],[0.7529412,0,1,1],[0.7568628,0,1,1],[0.7647059,0,1,1],[0.7686275,0,1,1],[0.7764706,0,1,1],[0.7803922,0,1,1],[0.7882353,0,1,1],[0.7921569,0,1,1],[0.8,0,1,1],[0.8078431,0,1,1],[0.8117647,0,1,1],[0.8196079,0,1,1],[0.8235294,0,1,1],[0.8313726,0,1,1],[0.8352941,0,1,1],[0.8431373,0,1,1],[0.8470588,0,1,1],[0.854902,0,1,1],[0.8588235,0,1,1],[0.8666667,0,1,1],[0.8705882,0,1,1],[0.8784314,0,1,1],[0.8823529,0,1,1],[0.8901961,0,1,1],[0.8941177,0,1,1],[0.9019608,0,1,1],[0.9098039,0,1,1],[0.9137255,0,1,1],[0.9215686,0,1,1],[0.9254902,0,1,1],[0.9333333,0,1,1],[0.9372549,0,1,1],[0.945098,0,1,1],[0.9490196,0,1,1],[0.9568627,0,1,1],[0.9607843,0,1,1],[0.9686275,0,1,1],[0.972549,0,1,1],[0.9803922,0,1,1],[0.9843137,0,1,1],[0.9921569,0,1,1],[0.9960784,0,1,1],[1,0,0.9960784,1],[1,0,0.9882353,1],[1,0,0.9843137,1],[1,0,0.9764706,1],[1,0,0.972549,1],[1,0,0.9647059,1],[1,0,0.9607843,1],[1,0,0.9529412,1],[1,0,0.9490196,1],[1,0,0.9411765,1],[1,0,0.9372549,1],[1,0,0.9294118,1],[1,0,0.9254902,1],[1,0,0.9176471,1],[1,0,0.9137255,1],[1,0,0.9058824,1],[1,0,0.9019608,1],[1,0,0.8941177,1],[1,0,0.8862745,1],[1,0,0.8823529,1],[1,0,0.8745098,1],[1,0,0.8705882,1],[1,0,0.8627451,1],[1,0,0.8588235,1],[1,0,0.8509804,1],[1,0,0.8470588,1],[1,0,0.8392157,1],[1,0,0.8352941,1],[1,0,0.827451,1],[1,0,0.8235294,1],[1,0,0.8156863,1],[1,0,0.8117647,1],[1,0,0.8039216,1],[1,0,0.7960784,1],[1,0,0.7921569,1],[1,0,0.7843137,1],[1,0,0.7803922,1],[1,0,0.772549,1],[1,0,0.7686275,1],[1,0,0.7607843,1],[1,0,0.7568628,1],[1,0,0.7490196,1],[1,0,0.7450981,1],[1,0,0.7372549,1],[1,0,0.7333333,1],[1,0,0.7254902,1],[1,0,0.7215686,1],[1,0,0.7137255,1],[1,0,0.7098039,1],[1,0,0.7019608,1],[1,0,0.6941177,1],[1,0,0.6901961,1],[1,0,0.682353,1],[1,0,0.6784314,1],[1,0,0.6705883,1],[1,0,0.6666667,1],[1,0,0.6588235,1],[1,0,0.654902,1],[1,0,0.6470588,1],[1,0,0.6431373,1],[1,0,0.6352941,1],[1,0,0.6313726,1],[1,0,0.6235294,1],[1,0,0.6196079,1],[1,0,0.6117647,1],[1,0,0.6078432,1],[1,0,0.6,1],[1,0,0.5921569,1],[1,0,0.5882353,1],[1,0,0.5803922,1],[1,0,0.5764706,1],[1,0,0.5686275,1],[1,0,0.5647059,1],[1,0,0.5568628,1],[1,0,0.5529412,1],[1,0,0.5450981,1],[1,0,0.5411765,1],[1,0,0.5333334,1],[1,0,0.5294118,1],[1,0,0.5215687,1],[1,0,0.5176471,1],[1,0,0.509804,1],[1,0,0.5058824,1],[1,0,0.4980392,1],[1,0,0.4901961,1],[1,0,0.4862745,1],[1,0,0.4784314,1],[1,0,0.4745098,1],[1,0,0.4666667,1],[1,0,0.4627451,1],[1,0,0.454902,1],[1,0,0.4509804,1],[1,0,0.4431373,1],[1,0,0.4392157,1],[1,0,0.4313726,1],[1,0,0.427451,1],[1,0,0.4196078,1],[1,0,0.4156863,1],[1,0,0.4078431,1],[1,0,0.4039216,1],[1,0,0.3960784,1],[1,0,0.3882353,1],[1,0,0.3843137,1],[1,0,0.3764706,1],[1,0,0.372549,1],[1,0,0.3647059,1],[1,0,0.3607843,1],[1,0,0.3529412,1],[1,0,0.3490196,1],[1,0,0.3411765,1],[1,0,0.3372549,1],[1,0,0.3294118,1],[1,0,0.3254902,1],[1,0,0.3176471,1],[1,0,0.3137255,1],[1,0,0.3058824,1],[1,0,0.2980392,1],[1,0,0.2941177,1],[1,0,0.2862745,1],[1,0,0.282353,1],[1,0,0.2745098,1],[1,0,0.2705882,1],[1,0,0.2627451,1],[1,0,0.2588235,1],[1,0,0.2509804,1],[1,0,0.2470588,1],[1,0,0.2392157,1],[1,0,0.2352941,1],[1,0,0.227451,1],[1,0,0.2235294,1],[1,0,0.2156863,1],[1,0,0.2117647,1],[1,0,0.2039216,1],[1,0,0.1960784,1],[1,0,0.1921569,1],[1,0,0.1843137,1],[1,0,0.1803922,1],[1,0,0.172549,1],[1,0,0.1686275,1],[1,0,0.1607843,1],[1,0,0.1568628,1],[1,0,0.1490196,1],[1,0,0.145098,1],[1,0,0.1372549,1],[1,0,0.1333333,1],[1,0,0.1254902,1],[1,0,0.1215686,1],[1,0,0.1137255,1],[1,0,0.1098039,1],[1,0,0.1019608,1],[1,0,0.09411765,1],[1,0,0.09019608,1],[1,0,0.08235294,1],[1,0,0.07843138,1],[1,0,0.07058824,1],[1,0,0.06666667,1],[1,0,0.05882353,1],[1,0,0.05490196,1],[1,0,0.04705882,1],[1,0,0.04313726,1],[1,0,0.03529412,1],[1,0,0.03137255,1],[1,0,0.02352941,1],[1,0,0.01960784,1],[1,0,0.01176471,1],[1,0,0.007843138,1]],"centers":[[-2.698091,-0.4080409,-1.76912],[-2.637779,-1.114133,-2.764644],[-2.631406,0.9478134,-2.266892],[-2.571282,0.4062715,-1.935792],[-2.520205,-2.007415,-0.2092482],[-2.49408,-1.15267,-0.6916792],[-2.488214,-0.454124,-2.327535],[-2.431154,0.5620464,-2.212973],[-2.279375,0.9095301,-1.258855],[-2.218185,0.8407594,-0.743318],[-2.206429,1.69437,-0.8344697],[-2.130762,0.2289791,-1.174041],[-2.125048,-1.236815,-3.305041],[-2.115046,1.36806,-0.1179394],[-2.100243,0.3919316,-0.1762427],[-2.07752,0.8435321,-1.432019],[-2.061161,-0.2003821,-1.952392],[-2.030655,0.1826273,-1.165502],[-2.022753,-0.3048269,-1.982972],[-2.005112,-0.4695318,-2.338596],[-1.997121,0.5552371,-0.7160828],[-1.962802,1.671005,-2.11938],[-1.956655,-0.389133,-3.699353],[-1.916015,-1.301409,-3.498494],[-1.863129,-0.5784134,-0.9827686],[-1.815447,-1.417874,-3.880229],[-1.813529,-0.3897848,-2.255027],[-1.811164,0.9880743,-0.4142622],[-1.805886,-1.742736,-1.658464],[-1.8029,1.324158,0.2519142],[-1.779614,-0.3091734,0.2186788],[-1.756822,-1.079334,-0.4388887],[-1.7378,-0.04048441,-0.5894462],[-1.698555,0.3416635,-1.697803],[-1.685394,1.468867,-0.9085705],[-1.681077,-2.311589,-2.135552],[-1.670356,-1.041247,-2.421996],[-1.666382,0.04949416,-2.595493],[-1.650653,0.2667539,-0.9970327],[-1.642211,0.007295691,-0.8015478],[-1.637707,0.6304171,-0.8641736],[-1.636384,0.9194499,-0.9028106],[-1.628015,0.3380439,-2.273988],[-1.624335,-1.050829,-2.267389],[-1.621563,1.14028,-1.406858],[-1.592113,-2.258818,-3.757425],[-1.557702,-0.4102478,-1.450684],[-1.548336,1.627731,-1.798639],[-1.54402,-0.4665647,-2.78739],[-1.52978,-1.291124,-1.442681],[-1.517074,1.118201,-1.113821],[-1.50925,0.4629269,0.4298691],[-1.5078,-0.3025064,-2.331395],[-1.488861,1.713611,-0.3755476],[-1.483055,-0.8805195,-0.6597895],[-1.471262,1.339724,-0.1739275],[-1.464529,-1.39897,-1.280993],[-1.461643,-1.46631,-3.487593],[-1.45946,-0.05830733,-2.81902],[-1.448041,0.2291663,-3.009178],[-1.446364,0.7843973,-1.319198],[-1.439575,-0.1453528,-3.104674],[-1.428142,1.480904,0.5984457],[-1.415928,-0.5270498,-0.948473],[-1.415109,1.073227,-2.547175],[-1.411382,-0.04508885,-1.773215],[-1.402826,1.142594,-1.351002],[-1.401057,-0.04655431,-0.72347],[-1.387316,0.3452573,-2.080706],[-1.381935,-1.560418,-0.9185835],[-1.373541,-0.1314221,-0.08847083],[-1.370575,2.067857,-0.287512],[-1.369273,-0.8930652,0.02985884],[-1.367462,1.512091,-0.5018241],[-1.349328,-0.3775712,-1.880146],[-1.34633,0.292183,-0.6123413],[-1.330106,-0.6147658,-3.395294],[-1.317121,0.724816,0.3045906],[-1.310972,-0.5056617,-3.342377],[-1.308796,-0.1353574,-3.12704],[-1.270029,0.9719494,-1.143129],[-1.264143,2.275384,-1.589488],[-1.2587,0.3231464,-2.30558],[-1.252624,-0.4447128,-0.8737275],[-1.243932,-0.7066392,-2.497147],[-1.243228,-0.2813906,-3.538901],[-1.237578,-2.181597,-3.566605],[-1.233066,1.653888,-1.710227],[-1.193051,-0.844381,-3.026067],[-1.190462,0.9669572,-1.470503],[-1.185031,0.4402163,-0.5088989],[-1.180425,0.7722347,-0.664161],[-1.180298,-0.7605204,-3.281185],[-1.173418,-1.086407,-0.3120941],[-1.16337,0.8124267,0.2462392],[-1.161243,-0.1282299,-3.766619],[-1.161177,-3.373272,-2.587497],[-1.160781,-1.937922,-1.497985],[-1.158506,-0.3211184,-1.951529],[-1.157862,-0.05703938,-0.4337835],[-1.143217,0.2255575,-0.5616106],[-1.143123,-0.447091,-0.9887124],[-1.139763,0.3682939,-1.996024],[-1.129897,-1.501088,-0.6557833],[-1.12388,-0.4682844,-4.0435],[-1.122168,-0.03295878,-1.630967],[-1.119234,-0.1074243,-0.07264001],[-1.118077,0.1334426,-1.806964],[-1.10721,-1.1483,-2.346153],[-1.105984,1.71596,-0.03061939],[-1.104531,-0.4978469,-1.153467],[-1.098507,-0.697894,-1.970042],[-1.097518,-1.991783,-2.179152],[-1.096223,1.592031,0.6123182],[-1.086095,-0.842384,-1.489882],[-1.082095,-0.1822308,-1.706206],[-1.06495,0.8619565,-1.438262],[-1.06061,-0.2620655,-1.748204],[-1.049157,0.6114593,-1.598771],[-1.039289,1.248997,-1.136132],[-1.036985,0.0200948,-0.1126768],[-1.033325,0.7954077,-0.03734311],[-1.028365,-0.9911182,-2.31225],[-1.027873,0.3923371,-0.5575404],[-1.025892,0.08114433,-1.813293],[-1.019427,-2.10445,-2.24143],[-1.015807,-1.004918,-3.219041],[-1.012472,0.2611223,-2.932039],[-1.011829,0.4337084,-2.612563],[-1.009016,2.178419,-0.85855],[-1.005545,1.025889,-0.8478997],[-1.005375,-0.5867808,-3.14396],[-1.004193,-1.433446,-4.077253],[-1.002042,-0.4859253,-0.9872164],[-0.9810383,-0.6501265,-1.822258],[-0.9798455,0.4308822,-2.060099],[-0.9789453,0.7486247,-1.257631],[-0.9769762,-1.313802,-3.294516],[-0.9735089,-0.3614621,-1.11302],[-0.9662206,2.130858,-1.78092],[-0.9608331,-0.1856087,-0.1610271],[-0.957248,1.009095,-3.014186],[-0.9522665,1.581114,-1.345273],[-0.9518015,-0.3178067,-1.418682],[-0.9511651,0.361706,-0.3702571],[-0.9493716,-0.03418205,-0.5940401],[-0.9454431,1.045498,-0.3392339],[-0.9414859,0.3879681,0.8728906],[-0.9366803,0.07056493,-0.6595999],[-0.935514,-0.6702787,-1.9789],[-0.9319037,0.4326209,-1.333417],[-0.9305358,1.035042,-0.3103826],[-0.9198744,-0.5414942,-2.390758],[-0.9194896,-0.07301731,-2.570297],[-0.9171023,1.682066,-0.1003172],[-0.9167874,1.069854,-0.9524096],[-0.9132155,-2.154278,-3.15969],[-0.9099437,0.8864462,-1.876207],[-0.9095918,-0.3562178,-1.135869],[-0.9065561,-1.294125,-2.785586],[-0.9063889,-1.213645,-1.960377],[-0.9058974,0.3764872,-2.600366],[-0.9051586,-1.202225,-3.177583],[-0.9020471,0.7040314,-3.000552],[-0.8908447,0.2778449,-1.529278],[-0.8882083,-2.041682,-3.441813],[-0.8881252,-1.159925,-3.360448],[-0.8838823,-1.202148,-3.824492],[-0.8747594,0.2988319,-1.459606],[-0.8738883,0.205947,-3.003015],[-0.8556798,-1.129688,-2.314965],[-0.8525199,-1.489681,-2.666042],[-0.8499789,-1.143675,-3.769358],[-0.8473707,0.4213509,0.200822],[-0.8451714,-0.6983646,-3.996256],[-0.835476,1.769531,-0.5116935],[-0.8195153,0.1656586,-2.499632],[-0.8188432,1.037585,-0.4478517],[-0.8165238,-1.258911,-2.609368],[-0.8119736,1.710424,0.3156086],[-0.8099792,-0.3742204,-1.790658],[-0.806048,2.363864,-1.722336],[-0.7896665,1.952395,1.671042],[-0.7823162,0.9081082,-1.06177],[-0.7820765,-1.164374,-2.589573],[-0.7790845,1.050596,-2.277492],[-0.7742296,0.06950713,-2.856788],[-0.7694524,-0.158636,-0.107083],[-0.7688795,2.195357,-0.08027815],[-0.7674901,0.5120604,-1.483922],[-0.7670977,0.5232366,-1.891829],[-0.7663085,0.9065226,-1.779113],[-0.7648789,0.12569,-2.587086],[-0.7604551,0.3619495,-1.407441],[-0.7604001,0.2363012,-0.9518843],[-0.7572739,0.7022168,-0.3559606],[-0.7546893,0.8385613,0.5923507],[-0.7393116,-1.029072,-1.486246],[-0.7370597,0.469158,-2.081596],[-0.734809,1.09493,-2.06259],[-0.7317324,-1.581961,-3.259968],[-0.7303221,-1.189968,-2.40447],[-0.7295313,0.04897291,-2.02366],[-0.7288415,0.6366655,-2.372563],[-0.7241259,-0.5264788,-3.277179],[-0.7228695,-0.5824551,-2.371223],[-0.7221947,0.2328071,-0.7156168],[-0.7184792,-2.711999,-4.731017],[-0.7139469,-0.9905515,-4.055326],[-0.7137544,0.3031659,-2.240981],[-0.7047544,-2.249536,-2.458701],[-0.7036772,0.1401746,-2.835915],[-0.6996373,-0.08898135,-1.193529],[-0.6894154,0.9054654,0.7917381],[-0.684857,-1.557857,-3.718032],[-0.6799761,0.4174405,0.06496211],[-0.6789712,0.599727,-1.240965],[-0.6774897,-1.402166,-4.588302],[-0.6770941,1.100089,0.3318267],[-0.6733527,0.5200213,-1.688498],[-0.6669374,1.628231,0.618764],[-0.6653032,1.272414,-0.001394736],[-0.6650122,-0.2865419,-1.081018],[-0.6576598,0.1932764,-1.52726],[-0.6566704,-0.6818507,-3.436878],[-0.654785,0.331051,-2.573356],[-0.653459,0.893591,0.1418726],[-0.6519669,-0.2263845,-2.523035],[-0.63763,0.3361015,-1.357811],[-0.6358801,0.3039913,-1.654265],[-0.6350691,-1.075634,-2.835335],[-0.634765,1.28776,0.7867885],[-0.6346654,0.7851076,-1.785953],[-0.6215844,-0.005072016,0.2192618],[-0.6203451,-1.019973,-2.098602],[-0.6200355,-0.8753759,-1.946508],[-0.6190248,-0.9561642,-2.75547],[-0.6158292,0.1411151,-1.921741],[-0.6156764,-0.8062926,-3.168903],[-0.6116183,1.028607,-0.5838537],[-0.6109166,0.8861306,-1.668317],[-0.6051939,-0.8399727,-4.110571],[-0.6033479,0.2029544,-0.9148265],[-0.6030278,0.7187622,-1.840425],[-0.5969848,-0.0763858,-3.786528],[-0.5934302,-0.01876669,-1.939486],[-0.5916129,0.549907,1.039174],[-0.5906686,-0.5276714,-3.170129],[-0.5895686,0.6571725,-0.4440079],[-0.5825979,-0.9070962,-2.705044],[-0.5777975,-1.93892,-2.54493],[-0.5713635,1.904418,-1.134574],[-0.5687147,-0.5536809,-2.577267],[-0.56762,0.1261611,0.2739055],[-0.5673387,0.2190202,-0.4730177],[-0.5635263,-0.8209667,-2.696876],[-0.5633803,0.6667966,-1.178007],[-0.5609791,-0.01147309,-3.718887],[-0.5566204,-1.121715,-3.239694],[-0.5547042,1.393735,1.373659],[-0.5506601,0.5574716,-0.1992434],[-0.5488752,-0.3848815,-0.9283347],[-0.5465384,0.336027,-1.493575],[-0.5424608,-0.7203184,-3.097332],[-0.5419819,0.1633648,-2.284838],[-0.537316,0.6292602,-2.197813],[-0.5314407,-0.1063861,-2.041021],[-0.5258936,1.554844,-0.3244994],[-0.518261,0.1668947,0.5646821],[-0.5168147,0.8651507,-1.586255],[-0.5124969,1.289725,-0.9342691],[-0.5093526,-1.002429,-3.384512],[-0.5081859,-0.3444803,-2.534862],[-0.5042306,-0.3676987,-3.424247],[-0.4991435,-1.29814,-3.256097],[-0.4938933,2.13863,-1.543794],[-0.4905628,-0.105808,-4.153374],[-0.4632948,-0.08956581,0.3671083],[-0.462242,-0.4452449,-3.678756],[-0.4598178,0.3123131,-1.563636],[-0.4579085,-1.171562,-3.050532],[-0.4571829,-1.155661,-2.62979],[-0.4555885,0.158503,-0.5967985],[-0.4505356,-0.2214887,-3.033619],[-0.4492,0.3477039,-2.788363],[-0.4451596,-0.6371142,-3.747141],[-0.4355633,0.7588331,0.3953794],[-0.4324713,1.822245,-0.3152229],[-0.4323308,0.9348637,0.003525726],[-0.430856,-2.10856,-2.340064],[-0.427603,0.7273439,-2.013025],[-0.427059,0.562022,0.9273976],[-0.4258322,-0.2263272,-2.398669],[-0.4255326,-0.7420785,-3.59415],[-0.4235487,-0.1536957,-1.700931],[-0.422997,-1.71553,-4.244068],[-0.4219474,-0.3947441,-1.503824],[-0.4199802,0.5638148,-1.877793],[-0.4198428,-0.7637995,-0.779233],[-0.4195411,0.7284507,0.9946264],[-0.4182427,0.7524051,1.309018],[-0.4130744,-0.5071849,-4.646426],[-0.4127306,-2.444218,-4.276469],[-0.4074863,1.740883,2.186598],[-0.4047172,-0.8157475,-2.592817],[-0.4042428,-0.6007112,-4.541303],[-0.4014494,-1.184638,-1.752691],[-0.401058,-0.1039305,-2.826417],[-0.4000622,1.063493,-1.470216],[-0.3996611,1.270032,0.003053421],[-0.3949125,0.6023921,0.60997],[-0.3904801,0.3549418,-1.148826],[-0.3901789,0.07323388,-0.4345543],[-0.3883133,0.6924286,-1.161256],[-0.3856604,0.7247332,-1.056151],[-0.3832497,-0.5446194,-1.550043],[-0.382416,2.515012,0.504063],[-0.3799231,0.1048764,-0.8248582],[-0.3736042,-2.381332,-1.602795],[-0.3722229,-0.6873915,-3.813433],[-0.3720607,1.467341,-0.09685422],[-0.3718465,0.004949955,-1.563512],[-0.3651185,1.497823,0.4107747],[-0.3632361,-0.7783043,-3.561366],[-0.362766,0.892673,-1.622595],[-0.3623515,0.07167102,-1.45435],[-0.3607741,0.271972,-1.752027],[-0.357387,1.654218,-0.8755962],[-0.3567552,-0.8000213,-3.316426],[-0.3547385,0.4989192,1.536792],[-0.3543888,0.9127979,0.2646095],[-0.3531546,0.5102568,-1.289154],[-0.3521383,1.574702,-0.2693968],[-0.3503703,-0.2655068,0.6225836],[-0.3477166,0.355396,1.151731],[-0.3461865,0.05690519,-2.289649],[-0.3396887,-0.7372035,-2.4137],[-0.3361985,-0.4073493,-2.09083],[-0.3322604,0.8130036,-0.3763643],[-0.3314615,0.6417056,-1.493562],[-0.3284412,0.573934,-0.01537053],[-0.3232373,0.3427506,-1.004992],[-0.3223144,-0.2264019,-1.800611],[-0.3220647,-1.4741,-1.575236],[-0.321702,0.6495224,-2.247921],[-0.3183335,-0.6583936,-5.111271],[-0.3168585,-1.138842,-4.789872],[-0.3130007,-0.3643673,-0.9059695],[-0.3080356,-1.261544,-2.445725],[-0.3078932,1.586707,0.8083549],[-0.3056579,2.735496,-1.314825],[-0.3007271,-1.496468,-3.315562],[-0.2988934,-0.4212792,-3.110469],[-0.2982858,1.092083,-2.092518],[-0.2953873,-1.59215,-2.87607],[-0.2953284,0.7905375,-0.0243004],[-0.2949135,0.936312,-0.6058944],[-0.2946482,-0.3392917,-4.119346],[-0.2914252,0.004957126,-3.652316],[-0.2861176,-0.7237874,-1.809586],[-0.2848259,-0.2593644,-4.637676],[-0.2785428,1.536434,-1.20908],[-0.2738813,0.3800033,0.1972911],[-0.266385,-0.8320367,-3.1875],[-0.2659507,0.371642,-3.00505],[-0.2657536,-1.142177,-2.34039],[-0.265693,0.8472182,0.9760464],[-0.2644366,1.691763,0.3982249],[-0.2644193,-0.8897371,-2.298346],[-0.2638619,-0.196309,-2.211884],[-0.2566052,0.466148,-0.9603851],[-0.2563379,-1.356335,-3.56431],[-0.2506005,-0.6312032,-3.611964],[-0.2491361,0.009636628,-1.127292],[-0.2464486,0.1022438,0.4969673],[-0.2439465,0.2984191,-0.4964958],[-0.2432354,2.217471,-0.3699724],[-0.240802,-0.2436822,-0.5246931],[-0.2372501,0.5421916,-1.817028],[-0.2372,0.1252587,-0.7952527],[-0.236009,0.97574,0.5739176],[-0.230422,0.7911978,-1.455905],[-0.2249817,-0.2890697,-3.044905],[-0.2233801,0.7753204,1.365036],[-0.2163164,-0.4230817,-5.502378],[-0.2140484,-1.243644,-3.485529],[-0.2116904,0.6959137,-0.3855352],[-0.2104816,0.07800603,-1.404323],[-0.2083904,0.3824568,0.1978458],[-0.2081065,-0.8628402,-2.072748],[-0.2044751,0.3017265,0.6082758],[-0.2026758,-1.075858,-1.981921],[-0.2008818,-0.8588784,-3.504908],[-0.2003343,0.6284487,0.248236],[-0.1950007,-0.0216812,-2.747977],[-0.1946933,0.7250155,-0.4237607],[-0.1943946,-0.944368,-4.211428],[-0.1930731,-0.4321227,-5.023059],[-0.1918401,0.2303864,-0.2319509],[-0.1846236,-1.221525,-1.953704],[-0.1709775,-1.24737,-2.288052],[-0.1698286,-0.09805026,-1.929898],[-0.1688243,-1.759035,-5.309244],[-0.1635815,-0.539978,-2.726539],[-0.1631199,-1.580539,-2.335995],[-0.1628721,1.310174,0.2468637],[-0.1623996,0.6034477,-0.3268369],[-0.1622692,-1.554209,-1.668662],[-0.1573042,0.2524717,-0.7656935],[-0.1557268,1.30069,1.879949],[-0.1557143,-0.7917352,-3.837595],[-0.1542536,0.8903826,-0.1487134],[-0.1496972,0.336609,-0.3723722],[-0.1435716,-1.261334,-2.708295],[-0.1432522,-1.936337,-3.666569],[-0.1390259,-0.7926073,-3.196707],[-0.1365294,-0.3529029,-3.888285],[-0.1358165,-0.3647227,-2.994819],[-0.1298983,1.965306,-0.4707854],[-0.1295084,1.794977,0.4262459],[-0.1217616,0.1149236,-1.59439],[-0.1193313,0.04491994,-0.7857456],[-0.1161639,1.44568,-1.690723],[-0.1077462,-0.2918859,-3.310585],[-0.1073995,0.1874958,0.9589948],[-0.1039017,-1.088678,-2.907706],[-0.1028396,1.536511,-0.0220005],[-0.1018052,0.01033358,-3.495059],[-0.09813314,0.8721092,-1.628011],[-0.09474822,1.722009,-0.5528703],[-0.09353525,-0.4729759,-2.207963],[-0.09261701,-0.1344131,-2.393118],[-0.09225906,0.2349937,-0.8614672],[-0.09051056,0.7477192,-1.083562],[-0.08122503,-0.3832135,-3.08911],[-0.08078411,0.2170746,-0.8727877],[-0.08017219,-1.848542,-2.707301],[-0.07588977,-0.7098573,-2.021685],[-0.07508266,-0.2019708,-2.57279],[-0.07265466,1.039457,0.6435497],[-0.07062629,-0.635135,-3.248814],[-0.07060023,-0.9374727,-4.931874],[-0.06870703,-0.9685352,-3.443696],[-0.06622264,-0.3668982,-0.658648],[-0.06565817,-1.493756,-3.532003],[-0.0634596,-1.094989,-2.883778],[-0.06286602,-0.3359954,-4.175107],[-0.06099125,-0.007517906,-1.523343],[-0.05996166,-0.5161564,-3.175692],[-0.05893727,0.2022601,-0.1632594],[-0.05818862,-0.2356097,-2.134159],[-0.0581149,-0.9975072,-4.219564],[-0.05707483,-0.2393312,-2.832616],[-0.05441905,0.1034779,-0.7086146],[-0.04316797,0.9889518,-0.344826],[-0.04024342,-0.8354899,-1.017092],[-0.03971476,1.114836,-0.8503198],[-0.03684003,-0.4247808,-1.956688],[-0.0273591,1.743472,-0.2624162],[-0.01645292,-1.238911,-2.655582],[-0.01010572,0.9220865,-0.6761742],[-0.008838071,1.326341,0.7621205],[-0.002313605,-0.068172,-2.710202],[0.001812595,0.7596155,-0.4834485],[0.003432657,1.077457,-0.7613002],[0.006883266,0.3561597,-1.895805],[0.007084948,1.470047,0.7047943],[0.008568361,1.0811,-0.6778371],[0.009386154,-1.590224,5.278797],[0.009499406,0.4922326,-0.693838],[0.01279531,0.3792539,-0.317825],[0.01293129,0.867364,-0.1079426],[0.02005621,0.610877,0.5612233],[0.02053929,-1.046772,3.686784],[0.02118182,0.4431384,0.1003768],[0.02205424,-0.8308192,3.218258],[0.02508244,1.346163,-0.5618241],[0.02847881,0.4516073,0.3055841],[0.03318108,1.284448,-2.517566],[0.03626271,-0.1929761,1.750634],[0.03951141,-1.559149,0.1389875],[0.04015632,0.8378853,1.104267],[0.04150062,-0.02785678,0.9155779],[0.04345989,-0.0005061282,-0.4774049],[0.04368677,0.6140209,-0.1840782],[0.04672507,-2.488326,3.194548],[0.04713364,0.9070284,-0.1178593],[0.048007,1.66789,0.7496345],[0.04950205,0.8418329,1.964226],[0.05759111,-0.9299825,3.548196],[0.05878518,0.2620185,-0.6564575],[0.06085861,0.9298986,1.734678],[0.06206696,-0.6784609,4.802381],[0.06774449,0.120316,1.692232],[0.06834574,-0.6603213,1.514189],[0.07345307,0.07259899,1.984072],[0.07904813,-0.4019794,1.804169],[0.07965027,1.013108,-0.05824713],[0.08401249,-0.6157677,3.305694],[0.08423501,1.870788,1.045576],[0.0848085,2.151655,-0.5486975],[0.08588469,0.3899944,1.422788],[0.08661131,0.3580842,-0.7143657],[0.0976164,1.125269,-1.100317],[0.09818124,0.001045209,1.314199],[0.1015329,0.1811291,0.474447],[0.1040705,0.2480165,0.6953393],[0.1041191,0.6186721,1.24741],[0.1051646,1.091265,-0.5246451],[0.1070454,0.9589002,0.3248403],[0.1116562,0.01632002,0.7462374],[0.1146098,-0.862379,2.993078],[0.1192865,0.9114322,0.07197124],[0.1197646,1.041851,-0.1843769],[0.1200723,-0.7677283,2.184489],[0.12073,-1.098498,2.988836],[0.1211344,-0.1036005,1.076609],[0.1215862,-0.09105407,2.000539],[0.1219391,1.665641,0.5219961],[0.1221875,1.688687,1.084745],[0.1251151,-2.635203,1.392912],[0.1284442,1.523833,-0.8597882],[0.1288203,0.817822,2.094497],[0.1343286,1.532055,-2.208463],[0.1359837,1.817955,-0.2682519],[0.1369409,1.218032,-1.929489],[0.1395478,-1.452,1.323458],[0.1398607,-0.332212,3.153262],[0.1462861,-0.964784,3.198637],[0.1518212,-0.1172259,1.638678],[0.1521644,0.2425358,1.816544],[0.1528199,-1.10007,2.440952],[0.1531148,-0.8698508,3.535704],[0.1569578,0.1990724,1.200775],[0.1582122,-1.066121,3.177363],[0.1588119,0.5911132,4.292241],[0.1621042,1.173005,1.196853],[0.1684612,-2.164667,3.321435],[0.1689034,-0.6567931,3.097414],[0.1715889,-0.6481953,3.802719],[0.1724113,0.3602505,-0.3235134],[0.1740109,-0.2393473,2.614601],[0.1756197,0.3411574,0.1105718],[0.1791185,1.263699,-0.1977402],[0.181284,0.5729772,1.659784],[0.1827165,-0.2600631,2.264797],[0.1885678,-0.1984457,2.8651],[0.1913959,-0.1591747,2.292788],[0.1940335,-1.670159,3.843458],[0.1953471,0.5709532,0.5800666],[0.1957086,-1.410069,2.49492],[0.1957262,-1.075089,1.931562],[0.1986117,-0.09004831,0.1758073],[0.1988815,0.5485587,0.6020435],[0.1991411,-0.1701113,2.829118],[0.2016805,0.4048135,0.1384804],[0.202189,1.242675,-0.6309758],[0.2031734,-0.4375891,2.998678],[0.2069724,1.215751,-0.9091671],[0.2139657,-0.5416396,2.360194],[0.2147508,-1.12056,2.553622],[0.2151123,-0.5117682,1.932628],[0.2162486,-0.3566471,4.714458],[0.2200797,-0.6716508,2.581077],[0.2268337,1.009351,2.071296],[0.228423,0.4660832,0.8225064],[0.2336136,0.1925828,2.515578],[0.2368285,-1.121868,1.635603],[0.2384231,0.7309346,-0.2894985],[0.2430142,-1.243578,3.106453],[0.244416,-0.4948615,3.223708],[0.24616,0.04172675,3.174353],[0.2466138,1.275117,0.5600567],[0.2505524,0.1334458,-0.2358251],[0.2508502,-1.079516,2.467839],[0.2570375,-1.341715,2.400166],[0.2598632,-0.3499724,3.007662],[0.2603341,0.7869673,0.1063933],[0.2658474,-1.2702,1.880655],[0.2697341,0.4466847,2.015799],[0.2704147,-0.4370944,3.301746],[0.2705155,0.09332839,1.175375],[0.2706131,1.838022,0.5245526],[0.2735542,-0.2049775,0.6447899],[0.27507,0.5275751,0.9391739],[0.276589,2.60855,-0.07501526],[0.2768284,-0.3354309,1.074475],[0.2795765,0.02891534,2.112596],[0.2860261,-0.5305522,2.665777],[0.2895666,0.04406894,2.646468],[0.2901129,-1.090124,1.727443],[0.2905112,-2.729313,3.443655],[0.2927979,-0.3876833,1.828834],[0.2929574,-0.1695271,4.052578],[0.2959261,-0.6607987,4.277666],[0.3011252,-0.3880153,1.958698],[0.3037549,-0.9752401,2.384552],[0.3050085,1.571636,0.11497],[0.3065719,-0.197489,1.024735],[0.30798,-0.7643813,2.584569],[0.309844,2.438415,-0.3420215],[0.3102541,-0.3404696,2.237585],[0.3102771,0.7934153,0.5106215],[0.3176392,0.2501701,0.186722],[0.320444,-0.3450103,3.083241],[0.3268745,-2.343349,2.90884],[0.3313397,-0.00528594,2.573796],[0.3332015,-0.4876401,1.952486],[0.3335301,0.8295106,-0.367959],[0.3352174,-0.1699234,2.970796],[0.3367765,-0.1780721,3.601411],[0.3367987,0.01659263,1.413836],[0.337074,-0.1465353,1.666172],[0.3408413,-1.947814,2.931629],[0.3423882,-0.3350493,1.6535],[0.3425475,-0.08980317,2.976043],[0.3447686,-0.09343401,0.65075],[0.3460817,0.92146,0.1411331],[0.3468923,0.2515321,3.135735],[0.3485621,-0.002988767,1.647349],[0.3544721,-0.6961325,1.392889],[0.3582797,1.324971,1.356916],[0.361293,-0.6787231,1.38406],[0.377406,-0.8295141,3.612067],[0.3774472,0.5461547,1.832012],[0.3802534,0.5473651,0.9398766],[0.3822131,0.09418357,1.835489],[0.3846381,-0.7070186,2.38037],[0.3857914,0.5253182,2.895688],[0.3864283,0.100635,-1.303319],[0.3883243,1.085542,1.30739],[0.3884434,-1.361676,4.219064],[0.3906546,0.4204496,0.2067005],[0.3926565,1.453383,0.3631559],[0.3959146,0.1180114,2.168771],[0.3966169,-0.01097088,1.780837],[0.3998742,0.3105161,1.327788],[0.4023733,1.061697,0.172362],[0.4024663,1.488258,0.2825376],[0.4049831,-0.1372966,1.450333],[0.4070815,1.623314,-0.1359093],[0.4072874,0.3574727,0.2821209],[0.4080309,-0.2023629,3.125954],[0.4087716,0.2616907,0.5092579],[0.4107708,0.484103,0.5652008],[0.4214772,-1.253253,2.433156],[0.4258258,1.732374,0.122647],[0.4271102,-0.3167876,2.584437],[0.4291143,-1.033394,2.411768],[0.4302541,1.685133,-1.18015],[0.430499,-1.1281,2.352909],[0.4306156,0.01386302,0.3007972],[0.4339869,-0.3189112,3.410777],[0.4358712,0.5250663,0.7003641],[0.437061,0.3250695,1.742687],[0.4380359,-0.4482374,2.332215],[0.4382087,1.112584,-0.1449097],[0.4391219,-0.1945085,3.03302],[0.4418333,0.07780182,1.867019],[0.4462044,-1.082819,1.769722],[0.4539274,-1.265427,2.674355],[0.4587994,2.826212,-0.2826629],[0.4661267,-0.6896868,1.637072],[0.4762367,-1.927014,4.772276],[0.4796323,0.1583006,0.003201153],[0.4914576,-0.5350713,3.541009],[0.4941262,1.555298,0.506023],[0.4985195,-0.9990884,3.208999],[0.5035469,0.00292291,0.9636858],[0.5036188,-1.076429,1.476316],[0.5037387,0.3403272,-0.8795208],[0.5040942,-0.6331748,3.138385],[0.5041246,0.6873325,-0.2538242],[0.5108652,-0.1971188,0.3919107],[0.5123335,0.9464762,-1.276282],[0.514573,-1.100777,2.041081],[0.5232197,-0.4754086,0.2669801],[0.5246616,0.5519691,1.592524],[0.5249542,1.882982,-1.29034],[0.5257537,0.121223,-0.1132431],[0.527084,1.184118,1.054984],[0.5288602,0.7104708,-0.4720231],[0.5292203,-1.174892,3.573759],[0.5293068,0.562215,-0.9993421],[0.529401,-0.1792114,2.176404],[0.5303524,-0.7702365,2.494879],[0.5331351,0.2659586,1.108809],[0.5357813,0.170885,-0.03434777],[0.5399267,-0.3927397,2.791984],[0.5475723,-2.428782,4.045996],[0.5506817,-1.534902,3.81695],[0.5509267,0.9349398,1.241572],[0.5526863,-0.2205319,0.6217749],[0.5528677,0.5277748,0.002925963],[0.5541881,0.3400356,2.220291],[0.5572597,0.9761556,0.5705109],[0.5609733,-0.3878081,3.166619],[0.5634833,-1.392774,2.424805],[0.5649506,-0.8097829,3.26117],[0.564993,0.669199,0.6019622],[0.5658673,-0.3291692,1.762526],[0.5701275,1.343877,0.4681846],[0.5706323,1.413484,0.8607],[0.5747957,-2.378414,3.17378],[0.5748736,2.118655,0.6636286],[0.5806504,1.766709,-0.8654594],[0.5957807,1.46292,0.1307891],[0.5977429,0.3664061,0.6620684],[0.6001395,-0.939175,2.460921],[0.6020398,-0.4710108,2.199513],[0.6072915,-0.7289116,2.971318],[0.6087049,-1.460716,2.975296],[0.6114322,-0.03350366,1.026004],[0.6117378,-0.7124367,2.140786],[0.6131028,0.04041529,1.648116],[0.6131378,-0.8553314,2.388083],[0.6138982,0.3109937,1.212934],[0.6143004,-0.3814468,0.5457022],[0.6150035,-0.02043513,0.05830723],[0.619427,-0.8325016,2.08549],[0.6207013,1.530461,-0.02719495],[0.623992,1.25185,-0.6681079],[0.6272979,0.2603117,0.5937074],[0.629252,-1.788529,3.727866],[0.6295785,2.263831,0.02995208],[0.631432,0.4893525,1.034901],[0.6324325,1.690956,-1.385897],[0.633716,-0.322964,1.611322],[0.6341376,-2.189028,2.874702],[0.6389514,-1.460427,1.406355],[0.640519,0.1133094,1.174263],[0.6465757,-1.033824,3.617109],[0.6504012,-0.132866,1.490862],[0.6506715,-0.07762062,0.684756],[0.6519278,0.83666,0.5362657],[0.6581618,-0.01902647,0.5112641],[0.6590939,1.174151,0.4029754],[0.6605209,-0.7730879,2.129319],[0.6620032,-0.927947,1.796711],[0.6652226,-0.4260406,2.989936],[0.6667013,-1.122087,2.09269],[0.6682823,-0.7570854,2.299217],[0.6685271,-1.735452,3.469269],[0.6708747,0.2973047,0.8147609],[0.6725983,0.5361043,0.2595777],[0.679687,-0.6284421,2.691635],[0.682505,1.911495,0.2539294],[0.689503,-0.3533106,1.735549],[0.6914628,-0.4657383,2.184181],[0.6945168,0.05506413,1.076715],[0.694911,-1.544917,2.248113],[0.698279,0.5849785,2.457001],[0.7007616,0.5318586,1.034361],[0.7040047,-0.4243233,2.24655],[0.7085949,0.1537888,2.054559],[0.7094249,-0.8849318,3.779192],[0.7115123,0.2259416,0.2595224],[0.7145249,0.4282376,0.9161701],[0.7174255,0.6872815,0.4225381],[0.7185168,0.263021,0.864014],[0.7261225,-0.9197111,1.095416],[0.731901,0.8647686,-0.501723],[0.7319885,-0.522621,0.9036686],[0.732135,1.380175,1.848474],[0.7367971,-1.696914,3.017326],[0.7402681,0.784453,0.2868021],[0.7490141,0.2157636,2.32671],[0.7498145,0.5384114,1.664881],[0.7515372,0.5584492,0.4332291],[0.7583815,0.884971,0.6900428],[0.7589937,0.06125825,1.622093],[0.7652376,-0.7173911,2.127562],[0.7707299,1.218393,-0.004672097],[0.7728253,-0.8522486,3.499798],[0.7739784,0.7527077,0.5252245],[0.7785763,-1.174398,2.499739],[0.7824852,0.05252993,3.488452],[0.7828597,-2.121817,1.851346],[0.7829956,-1.10839,2.350967],[0.7859297,-0.05734969,2.270116],[0.787077,-1.21577,3.681608],[0.7874984,1.919383,1.056439],[0.7897992,1.236324,0.5262888],[0.7936214,-1.370202,2.379059],[0.79371,0.5438022,0.983529],[0.8037671,-1.938315,4.682467],[0.8053132,0.8168406,0.9626284],[0.8086342,-0.4711715,1.508469],[0.8168249,-0.7385294,2.987521],[0.8187673,-0.05200801,2.177418],[0.8191684,0.1302,0.8359406],[0.820609,0.8463227,-0.3790026],[0.8210493,-0.09368182,2.934045],[0.8236374,-1.039212,3.812617],[0.8365081,-0.3142788,1.168845],[0.8378435,-1.877236,2.04246],[0.8388656,0.03910939,3.054918],[0.8392184,-0.278073,2.362769],[0.8429994,0.08140069,1.448758],[0.8449441,-0.7162265,3.49184],[0.8471092,0.3277474,1.399607],[0.8478581,-1.356214,3.826423],[0.8480466,-0.0006776184,1.980172],[0.8493193,-1.729952,4.762691],[0.8517476,0.9358752,0.3762315],[0.8536209,0.1083648,1.286754],[0.8551846,0.5611799,0.2664768],[0.8600633,0.9236026,-0.7545143],[0.8607278,-0.5300078,3.889536],[0.8709834,0.9155363,1.353194],[0.8790511,-1.261532,3.621005],[0.8794314,0.8833605,1.670024],[0.8827633,-0.264115,1.4215],[0.8836516,1.527928,0.07822686],[0.8901792,1.743508,0.06968565],[0.8902851,1.179011,-0.3770482],[0.898315,-1.341383,0.8318654],[0.9052142,0.1577815,1.537428],[0.9251912,0.5696683,0.01032451],[0.9263664,-0.3258111,3.037855],[0.9291855,0.8950209,1.087441],[0.9399319,-0.01522179,1.92121],[0.9417318,-1.074945,2.52898],[0.9456897,0.3196077,0.6187835],[0.9472497,-0.2953741,0.5150495],[0.9510632,-1.319862,0.4516211],[0.9590859,-0.04199061,3.322856],[0.9608966,1.72986,0.5333407],[0.9645988,0.01078481,1.032337],[0.9706286,-0.02768979,2.226699],[0.9745019,2.15598,-0.1951266],[0.9790424,-0.1943412,3.194515],[0.9798916,-0.1350506,0.3051732],[1.001038,0.1922232,2.31716],[1.003506,-0.6672732,0.3559859],[1.005136,1.310082,-0.2724518],[1.00604,1.235212,0.1636746],[1.009257,0.818991,0.798971],[1.009898,-0.09889916,1.489801],[1.010851,1.461258,0.830278],[1.012594,0.7260576,0.3863858],[1.013735,-2.024965,2.741685],[1.0148,1.867101,0.4778246],[1.026486,0.444443,1.148251],[1.032578,3.929453,0.6772913],[1.038344,0.4556497,0.9920177],[1.043534,-1.182491,3.355423],[1.049209,-1.48341,3.486662],[1.049764,0.0001563914,2.843569],[1.050058,-2.565974,3.398218],[1.051728,-0.1914604,2.457246],[1.065887,0.5080581,-0.03352394],[1.083362,-0.5346677,3.389852],[1.08544,0.6371027,0.5802428],[1.091124,1.619805,1.019797],[1.098061,-0.8401135,3.310541],[1.108472,0.5857105,1.633426],[1.112721,-1.658852,2.895774],[1.116165,-0.8754113,1.216229],[1.123223,1.010192,-0.09042454],[1.123498,1.189096,2.144435],[1.131314,1.844496,-1.067471],[1.137105,-2.108459,0.4211389],[1.14248,0.1386632,2.490434],[1.149183,1.168466,0.06934381],[1.152966,-0.4978215,3.400487],[1.155059,-0.3947623,0.5271118],[1.157787,-1.961428,1.769382],[1.16182,0.5152891,1.385135],[1.169896,-2.110233,3.756084],[1.170793,0.3439417,1.505593],[1.170854,-0.8733917,2.256283],[1.171709,-0.4323568,2.323275],[1.175518,-0.0129686,2.943067],[1.180016,0.1841998,1.143342],[1.180432,0.0960728,2.041894],[1.184007,0.02907228,2.63708],[1.193166,-1.317384,2.945866],[1.19443,-0.2070305,0.4541642],[1.196899,0.7174106,1.346149],[1.200341,1.080394,1.401444],[1.209206,0.710912,0.4147272],[1.209287,-0.2682787,2.661009],[1.217167,-0.3099277,3.092848],[1.227294,0.08106135,2.370942],[1.227368,-0.560647,1.269849],[1.232824,-0.6741372,2.33677],[1.234102,-0.07629745,0.7653582],[1.237428,-0.4043078,1.000211],[1.240998,0.8320726,1.122382],[1.241493,-0.07657186,2.559841],[1.245285,-1.853403,2.861397],[1.25076,0.7453831,0.5228604],[1.252245,-0.1682813,3.063012],[1.25373,1.008792,-0.6239855],[1.25886,0.09677546,2.147908],[1.264579,0.4327875,3.699487],[1.270713,-0.4295955,0.850283],[1.28631,-0.1717996,1.329924],[1.291406,-0.8607772,1.923424],[1.29263,-0.7982724,2.794434],[1.296311,0.2883966,2.673794],[1.301249,-0.1600685,0.6552262],[1.311234,-1.091398,1.978164],[1.314118,-1.575289,3.569396],[1.316715,0.7825914,1.631115],[1.33678,0.08512583,1.530362],[1.339389,1.424475,0.1552918],[1.340501,1.257537,-0.08586977],[1.346658,0.4381994,2.690944],[1.34814,2.13171,1.368518],[1.35,0.3796011,0.8665748],[1.354288,-0.410195,0.6183834],[1.365495,-0.3579386,0.2867709],[1.36778,-0.7031512,1.516487],[1.370116,-0.5280742,0.8111314],[1.376507,-0.1288136,2.337526],[1.381366,1.749755,0.1173815],[1.383291,1.711621,0.03486221],[1.389437,-0.9601376,2.209979],[1.390774,1.492439,1.071004],[1.39924,0.392227,-0.4341336],[1.408319,0.1224214,2.170386],[1.41212,-0.117387,-0.2661201],[1.420146,-0.1808504,1.56945],[1.420246,0.9336858,-0.217022],[1.428311,-0.9010071,3.063232],[1.430947,0.08613319,2.061031],[1.434135,0.3500811,1.829485],[1.450235,0.02845597,3.360028],[1.456256,0.01605218,0.8965483],[1.457709,-1.466699,3.77387],[1.461437,1.000209,0.2904622],[1.464231,0.4363409,2.921478],[1.465059,1.707238,0.06218401],[1.465343,2.316457,1.079146],[1.467998,-0.7935233,2.101578],[1.485586,-0.009334872,2.775597],[1.489327,0.0731075,-0.8102003],[1.49157,1.280403,0.8012057],[1.491677,0.2259324,1.848422],[1.51439,0.2834936,2.849153],[1.533839,0.03451316,1.808234],[1.536489,1.35683,0.0939148],[1.53868,-0.3772756,0.8359445],[1.542996,0.4326922,1.219797],[1.546921,0.8864947,2.361369],[1.558613,-0.3036741,3.857315],[1.595239,0.5506546,2.048374],[1.614082,0.04368478,-0.006941631],[1.619569,1.240772,-0.3618338],[1.621916,2.045002,3.8168],[1.655582,-0.09464794,1.917448],[1.65742,-1.007649,1.934678],[1.660047,0.09451885,2.396542],[1.685994,-1.088573,0.4929277],[1.699416,-1.31965,3.762499],[1.703572,1.42724,1.21735],[1.724865,0.4607578,2.679675],[1.751509,1.215007,2.816626],[1.762631,-1.852116,1.242268],[1.763343,-0.5073424,3.049484],[1.77172,-0.5221603,0.98738],[1.810705,0.2762617,0.03110171],[1.811237,0.08419959,0.9628761],[1.816308,-1.669909,3.036588],[1.829711,-0.1963245,0.7179686],[1.832075,0.1642444,2.065993],[1.841522,0.8321584,2.145656],[1.849665,-0.04614455,2.810349],[1.852056,0.8573142,-0.4961838],[1.862955,0.652155,-0.7340718],[1.870465,-0.3404808,3.981937],[1.888949,1.36021,1.59528],[1.907029,0.5373367,1.763463],[1.919762,-0.2815144,2.289186],[1.929926,0.3805474,1.577436],[1.937357,0.3677595,-0.866981],[1.952521,-0.7366079,3.457722],[1.970138,2.339844,-0.4397327],[1.976849,0.02831855,0.8026706],[1.984188,0.02518677,3.347633],[1.998785,0.3870761,0.9632369],[2.018219,0.6992081,0.7087033],[2.052705,-0.1838838,2.874257],[2.122437,-0.6115611,1.718803],[2.141348,0.01007263,2.219761],[2.142747,0.06405229,0.9034356],[2.177831,-0.08335208,0.2058818],[2.216152,0.2915374,2.429371],[2.272345,-2.408828,3.979109],[2.298579,-1.603539,1.262664],[2.304026,1.31907,-0.9915661],[2.419471,-0.6271333,0.4266405],[2.45774,-0.5470855,2.114122],[2.475149,2.321609,-0.8786085],[2.480019,0.04906114,1.297351],[2.607543,-0.2347621,0.008657314],[2.707211,-0.5342721,2.258714],[2.723716,0.729841,1.430411]],"ignoreExtent":false,"flags":8192},"9":{"id":9,"type":"text","material":{"lit":false},"vertices":[[0.0128125,-4.611084,-7.329787]],"colors":[[0,0,0,1]],"texts":[["x"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[0.0128125,-4.611084,-7.329787]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":4136},"10":{"id":10,"type":"text","material":{"lit":false},"vertices":[[-3.617087,0.2780905,-7.329787]],"colors":[[0,0,0,1]],"texts":[["y"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[-3.617087,0.2780905,-7.329787]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":4136},"11":{"id":11,"type":"text","material":{"lit":false},"vertices":[[-3.617087,-4.611084,-0.1117907]],"colors":[[0,0,0,1]],"texts":[["z"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[-3.617087,-4.611084,-0.1117907]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":4136},"5":{"id":5,"type":"light","vertices":[[0,0,1]],"colors":[[1,1,1,1],[1,1,1,1],[1,1,1,1]],"viewpoint":true,"finite":false},"4":{"id":4,"type":"background","material":{"fog":true},"colors":[[0.2980392,0.2980392,0.2980392,1]],"centers":[[0,0,0]],"sphere":false,"fogtype":"none","flags":0},"6":{"id":6,"type":"background","material":{"lit":false,"back":"lines"},"colors":[[1,1,1,1]],"centers":[[0,0,0]],"sphere":false,"fogtype":"none","flags":0},"8":{"id":8,"type":"bboxdeco","material":{"front":"lines","back":"lines"},"vertices":[[-2,"NA","NA"],[-1,"NA","NA"],[0,"NA","NA"],[1,"NA","NA"],[2,"NA","NA"],["NA",-2,"NA"],["NA",0,"NA"],["NA",2,"NA"],["NA","NA",-4],["NA","NA",-2],["NA","NA",0],["NA","NA",2],["NA","NA",4]],"colors":[[0,0,0,1]],"draw_front":true,"newIds":[19,20,21,22,23,24,25]},"1":{"id":1,"type":"subscene","par3d":{"antialias":8,"FOV":30,"ignoreExtent":false,"listeners":1,"mouseMode":{"left":"trackball","right":"zoom","middle":"fov","wheel":"pull"},"observer":[0,0,34.51584],"modelMatrix":[[1.502027,0,0,-0.01924489],[0,0.3814069,0.7098089,-0.02671561],[0,-1.047907,0.2583493,-34.19555],[0,0,0,1]],"projMatrix":[[2.665751,0,0,0],[0,3.732051,0,0],[0,0,-3.863703,-124.4256],[0,0,-1,0]],"skipRedraw":false,"userMatrix":[[1,0,0,0],[0,0.3420201,0.9396926,0],[0,-0.9396926,0.3420201,0],[0,0,0,1]],"scale":[1.502027,1.115159,0.7553629],"viewport":{"x":0,"y":0,"width":1,"height":1},"zoom":1,"bbox":[-2.698091,2.723716,-3.373272,3.929453,-5.502378,5.278797],"windowRect":[4,45,676,525],"family":"sans","font":1,"cex":1,"useFreeType":true,"fontname":"/Library/Frameworks/R.framework/Versions/3.4/Resources/library/rgl/fonts/FreeSans.ttf","maxClipPlanes":6,"glVersion":2.1},"embeddings":{"viewport":"replace","projection":"replace","model":"replace"},"objects":[6,8,7,9,10,11,5,19,20,21,22,23,24,25],"subscenes":[],"flags":13480},"19":{"id":19,"type":"lines","material":{"lit":false,"front":"lines","back":"lines"},"vertices":[[-2,-3.482813,-5.664095],[2,-3.482813,-5.664095],[-2,-3.482813,-5.664095],[-2,-3.670858,-5.941711],[-1,-3.482813,-5.664095],[-1,-3.670858,-5.941711],[0,-3.482813,-5.664095],[0,-3.670858,-5.941711],[1,-3.482813,-5.664095],[1,-3.670858,-5.941711],[2,-3.482813,-5.664095],[2,-3.670858,-5.941711]],"colors":[[0,0,0,1]],"centers":[[0,-3.482813,-5.664095],[-2,-3.576835,-5.802903],[-1,-3.576835,-5.802903],[0,-3.576835,-5.802903],[1,-3.576835,-5.802903],[2,-3.576835,-5.802903]],"ignoreExtent":true,"origId":8,"flags":128},"20":{"id":20,"type":"text","material":{"lit":false,"front":"lines","back":"lines"},"vertices":[[-2,-4.046948,-6.496942],[-1,-4.046948,-6.496942],[0,-4.046948,-6.496942],[1,-4.046948,-6.496942],[2,-4.046948,-6.496942]],"colors":[[0,0,0,1]],"texts":[["-2"],["-1"],["0"],["1"],["2"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[-2,-4.046948,-6.496942],[-1,-4.046948,-6.496942],[0,-4.046948,-6.496942],[1,-4.046948,-6.496942],[2,-4.046948,-6.496942]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"origId":8,"flags":4136},"21":{"id":21,"type":"lines","material":{"lit":false,"front":"lines","back":"lines"},"vertices":[[-2.779418,-2,-5.664095],[-2.779418,2,-5.664095],[-2.779418,-2,-5.664095],[-2.91903,-2,-5.941711],[-2.779418,0,-5.664095],[-2.91903,0,-5.941711],[-2.779418,2,-5.664095],[-2.91903,2,-5.941711]],"colors":[[0,0,0,1]],"centers":[[-2.779418,0,-5.664095],[-2.849224,-2,-5.802903],[-2.849224,0,-5.802903],[-2.849224,2,-5.802903]],"ignoreExtent":true,"origId":8,"flags":128},"22":{"id":22,"type":"text","material":{"lit":false,"front":"lines","back":"lines"},"vertices":[[-3.198253,-2,-6.496942],[-3.198253,0,-6.496942],[-3.198253,2,-6.496942]],"colors":[[0,0,0,1]],"texts":[["-2"],["0"],["2"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[-3.198253,-2,-6.496942],[-3.198253,0,-6.496942],[-3.198253,2,-6.496942]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"origId":8,"flags":4136},"23":{"id":23,"type":"lines","material":{"lit":false,"front":"lines","back":"lines"},"vertices":[[-2.779418,-3.482813,-4],[-2.779418,-3.482813,4],[-2.779418,-3.482813,-4],[-2.91903,-3.670858,-4],[-2.779418,-3.482813,-2],[-2.91903,-3.670858,-2],[-2.779418,-3.482813,0],[-2.91903,-3.670858,0],[-2.779418,-3.482813,2],[-2.91903,-3.670858,2],[-2.779418,-3.482813,4],[-2.91903,-3.670858,4]],"colors":[[0,0,0,1]],"centers":[[-2.779418,-3.482813,0],[-2.849224,-3.576835,-4],[-2.849224,-3.576835,-2],[-2.849224,-3.576835,0],[-2.849224,-3.576835,2],[-2.849224,-3.576835,4]],"ignoreExtent":true,"origId":8,"flags":128},"24":{"id":24,"type":"text","material":{"lit":false,"front":"lines","back":"lines"},"vertices":[[-3.198253,-4.046948,-4],[-3.198253,-4.046948,-2],[-3.198253,-4.046948,0],[-3.198253,-4.046948,2],[-3.198253,-4.046948,4]],"colors":[[0,0,0,1]],"texts":[["-4"],["-2"],["0"],["2"],["4"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[-3.198253,-4.046948,-4],[-3.198253,-4.046948,-2],[-3.198253,-4.046948,0],[-3.198253,-4.046948,2],[-3.198253,-4.046948,4]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"origId":8,"flags":4136},"25":{"id":25,"type":"lines","material":{"lit":false,"front":"lines","back":"lines"},"vertices":[[-2.779418,-3.482813,-5.664095],[-2.779418,4.038994,-5.664095],[-2.779418,-3.482813,5.440514],[-2.779418,4.038994,5.440514],[-2.779418,-3.482813,-5.664095],[-2.779418,-3.482813,5.440514],[-2.779418,4.038994,-5.664095],[-2.779418,4.038994,5.440514],[-2.779418,-3.482813,-5.664095],[2.805043,-3.482813,-5.664095],[-2.779418,-3.482813,5.440514],[2.805043,-3.482813,5.440514],[-2.779418,4.038994,-5.664095],[2.805043,4.038994,-5.664095],[-2.779418,4.038994,5.440514],[2.805043,4.038994,5.440514],[2.805043,-3.482813,-5.664095],[2.805043,4.038994,-5.664095],[2.805043,-3.482813,5.440514],[2.805043,4.038994,5.440514],[2.805043,-3.482813,-5.664095],[2.805043,-3.482813,5.440514],[2.805043,4.038994,-5.664095],[2.805043,4.038994,5.440514]],"colors":[[0,0,0,1]],"centers":[[-2.779418,0.2780905,-5.664095],[-2.779418,0.2780905,5.440514],[-2.779418,-3.482813,-0.1117907],[-2.779418,4.038994,-0.1117907],[0.0128125,-3.482813,-5.664095],[0.0128125,-3.482813,5.440514],[0.0128125,4.038994,-5.664095],[0.0128125,4.038994,5.440514],[2.805043,0.2780905,-5.664095],[2.805043,0.2780905,5.440514],[2.805043,-3.482813,-0.1117907],[2.805043,4.038994,-0.1117907]],"ignoreExtent":true,"origId":8,"flags":128}},"snapshot":"data:image/png;base64,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UQIx4ImIiBKIAU9ERJRADHiihFuxYsXkyZP37t0b/Lpjx45JkybV1dWVd6uIqNQY8EQJ19jYOGTIkDFjxgS/XnbZZcOGDcvkPRElFQOeKPlmz57dv3//JUuWvPbaa4cddtjy5cvLvUVEVHIMeKKKIA33oUOHHn/88XfeeWe5t4WIegMDnqgi7Ny588gjj6ypqWlubi73thBRb2DAE1WE3//+9wMGDOjXr9+bb75Z7m0hot7AgCdKvq1btw4ePHjChAmXXnrpCSecsHv37nJvERGVHAOeKPnOP//8kSNHtrS0NDY2Hnvssddee225t4iISo4BT5RwkydPHjBgwKpVq4JfX3zxxX79+r311lvl3SoiKjUGPBERUQIx4ImIiBKIAU9ERJRADHgiIqIEYsATERElEAOeiIgogRjwRERECcSAJyIiSiAGPBERUQIx4ImIiBKIAU9ERJRADHgiIqIEYsATERElEAOeiIgogRjwRERECcSAJyIiSiAGPBERUQIx4ImIiBKIAU9ERJRADHgiIqIEYsATERElEAOeiIgogRjwRERECcSAJyIiSiAGPBERUQIx4ImIiBKIAU9ERJRADHgiIqIEYsATERElEAOeiIgogRjwRERECcSAJyIiSiAGPBERUQL9f/nxCWp9FjBQAAAAAElFTkSuQmCC","width":673,"height":481,"sphereVerts":{"vb":[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.07465783,0.1464466,0.2126075,0.2705981,0.3181896,0.3535534,0.3753303,0.3826834,0.3753303,0.3535534,0.3181896,0.2705981,0.2126075,0.1464466,0.07465783,0,0,0.1379497,0.2705981,0.3928475,0.5,0.5879378,0.6532815,0.6935199,0.7071068,0.6935199,0.6532815,0.5879378,0.5,0.3928475,0.2705981,0.1379497,0,0,0.18024,0.3535534,0.51328,0.6532815,0.7681778,0.8535534,0.9061274,0.9238795,0.9061274,0.8535534,0.7681778,0.6532815,0.51328,0.3535534,0.18024,0,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,0.9807853,0.9238795,0.8314696,0.7071068,0.5555702,0.3826834,0.1950903,0,0,0.18024,0.3535534,0.51328,0.6532815,0.7681778,0.8535534,0.9061274,0.9238795,0.9061274,0.8535534,0.7681778,0.6532815,0.51328,0.3535534,0.18024,0,0,0.1379497,0.2705981,0.3928475,0.5,0.5879378,0.6532815,0.6935199,0.7071068,0.6935199,0.6532815,0.5879378,0.5,0.3928475,0.2705981,0.1379497,0,0,0.07465783,0.1464466,0.2126075,0.2705981,0.3181896,0.3535534,0.3753303,0.3826834,0.3753303,0.3535534,0.3181896,0.2705981,0.2126075,0.1464466,0.07465783,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-0,-0.07465783,-0.1464466,-0.2126075,-0.2705981,-0.3181896,-0.3535534,-0.3753303,-0.3826834,-0.3753303,-0.3535534,-0.3181896,-0.2705981,-0.2126075,-0.1464466,-0.07465783,-0,-0,-0.1379497,-0.2705981,-0.3928475,-0.5,-0.5879378,-0.6532815,-0.6935199,-0.7071068,-0.6935199,-0.6532815,-0.5879378,-0.5,-0.3928475,-0.2705981,-0.1379497,-0,-0,-0.18024,-0.3535534,-0.51328,-0.6532815,-0.7681778,-0.8535534,-0.9061274,-0.9238795,-0.9061274,-0.8535534,-0.7681778,-0.6532815,-0.51328,-0.3535534,-0.18024,-0,-0,-0.1950903,-0.3826834,-0.5555702,-0.7071068,-0.8314696,-0.9238795,-0.9807853,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,-0,-0,-0.18024,-0.3535534,-0.51328,-0.6532815,-0.7681778,-0.8535534,-0.9061274,-0.9238795,-0.9061274,-0.8535534,-0.7681778,-0.6532815,-0.51328,-0.3535534,-0.18024,-0,-0,-0.1379497,-0.2705981,-0.3928475,-0.5,-0.5879378,-0.6532815,-0.6935199,-0.7071068,-0.6935199,-0.6532815,-0.5879378,-0.5,-0.3928475,-0.2705981,-0.1379497,-0,-0,-0.07465783,-0.1464466,-0.2126075,-0.2705981,-0.3181896,-0.3535534,-0.3753303,-0.3826834,-0.3753303,-0.3535534,-0.3181896,-0.2705981,-0.2126075,-0.1464466,-0.07465783,-0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1],[0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,0.9807853,0.9238795,0.8314696,0.7071068,0.5555702,0.3826834,0.1950903,0,0,0.18024,0.3535534,0.51328,0.6532815,0.7681778,0.8535534,0.9061274,0.9238795,0.9061274,0.8535534,0.7681778,0.6532815,0.51328,0.3535534,0.18024,0,0,0.1379497,0.2705981,0.3928475,0.5,0.5879378,0.6532815,0.6935199,0.7071068,0.6935199,0.6532815,0.5879378,0.5,0.3928475,0.2705981,0.1379497,0,0,0.07465783,0.1464466,0.2126075,0.2705981,0.3181896,0.3535534,0.3753303,0.3826834,0.3753303,0.3535534,0.3181896,0.2705981,0.2126075,0.1464466,0.07465783,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-0,-0.07465783,-0.1464466,-0.2126075,-0.2705981,-0.3181896,-0.3535534,-0.3753303,-0.3826834,-0.3753303,-0.3535534,-0.3181896,-0.2705981,-0.2126075,-0.1464466,-0.07465783,-0,-0,-0.1379497,-0.2705981,-0.3928475,-0.5,-0.5879378,-0.6532815,-0.6935199,-0.7071068,-0.6935199,-0.6532815,-0.5879378,-0.5,-0.3928475,-0.2705981,-0.1379497,-0,-0,-0.18024,-0.3535534,-0.51328,-0.6532815,-0.7681778,-0.8535534,-0.9061274,-0.9238795,-0.9061274,-0.8535534,-0.7681778,-0.6532815,-0.51328,-0.3535534,-0.18024,-0,-0,-0.1950903,-0.3826834,-0.5555702,-0.7071068,-0.8314696,-0.9238795,-0.9807853,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,-0,-0,-0.18024,-0.3535534,-0.51328,-0.6532815,-0.7681778,-0.8535534,-0.9061274,-0.9238795,-0.9061274,-0.8535534,-0.7681778,-0.6532815,-0.51328,-0.3535534,-0.18024,-0,-0,-0.1379497,-0.2705981,-0.3928475,-0.5,-0.5879378,-0.6532815,-0.6935199,-0.7071068,-0.6935199,-0.6532815,-0.5879378,-0.5,-0.3928475,-0.2705981,-0.1379497,-0,-0,-0.07465783,-0.1464466,-0.2126075,-0.2705981,-0.3181896,-0.3535534,-0.3753303,-0.3826834,-0.3753303,-0.3535534,-0.3181896,-0.2705981,-0.2126075,-0.1464466,-0.07465783,-0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.07465783,0.1464466,0.2126075,0.2705981,0.3181896,0.3535534,0.3753303,0.3826834,0.3753303,0.3535534,0.3181896,0.2705981,0.2126075,0.1464466,0.07465783,0,0,0.1379497,0.2705981,0.3928475,0.5,0.5879378,0.6532815,0.6935199,0.7071068,0.6935199,0.6532815,0.5879378,0.5,0.3928475,0.2705981,0.1379497,0,0,0.18024,0.3535534,0.51328,0.6532815,0.7681778,0.8535534,0.9061274,0.9238795,0.9061274,0.8535534,0.7681778,0.6532815,0.51328,0.3535534,0.18024,0,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,0.9807853,0.9238795,0.8314696,0.7071068,0.5555702,0.3826834,0.1950903,0]],"it":[[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270],[17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288],[18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271]],"primitivetype":"triangle","material":null,"normals":null,"texcoords":[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],[0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1]]}});
make_webgl_plotrgl.prefix = "make_webgl_plot";
</script>
<p id="make_webgl_plotdebug">
You must enable Javascript to view this page properly.
</p>
<script>make_webgl_plotrgl.start();</script>
