"""
T(n) = 1 T(n/2) + O(1)

a = 1, b = 2, d = 0

Din teorema master timpul este log2 (n)
"""


def find_fixed_point(arr, left, right):
    length = right - left

    if length == 1:
        if arr[left] == left:
            return left
        else:
            return -1

    mid = left + length // 2

    if arr[mid] < mid:
        return find_fixed_point(arr, mid + 1, right)
    elif arr[mid] > mid:
        return find_fixed_point(arr, left, mid)
    else:
        return mid


fin = open('fix.txt')

n = int(next(fin))
nums = [int(x) for x in next(fin).split()]

fixed = find_fixed_point(nums, 0, n)

print(fixed)
