% Example 7.10, Thanh Doan
data = load('T7-3.DAT');
n = size(data,1);
y2 = [301.8, 396.1, 328.2, 307.4, 362.4, 369.5, 229.1]';
X = [data, y2];

save example7_10.data X -ascii -tabs;