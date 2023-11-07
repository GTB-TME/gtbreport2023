function out = get_numbers(vec)

vec2 = strrep(vec, ' ', '');

tmp1 = strsplit(vec2, '[');
num1 = str2double(tmp1{1});

tmp2 = strsplit(tmp1{2}, '-');
num2 = str2double(tmp2{1});

tmp3 = strsplit(tmp2{2}, ']');
num3 = str2double(tmp3{1});

out  = [num2, num1, num3];
