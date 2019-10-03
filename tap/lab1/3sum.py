fin = open('3sum.txt')

lines = map(str.strip, fin)

n = int(next(lines))
nums = map(int, next(lines).split())
nums = list(nums)

target = 0
seen = set()

nums.sort()

for i in range(n):
    # Cu primul element fixat, caut celelalte două elemente
    # care în total să dea suma 0
    left, right = i + 1, n - 1
    while left < right:
        solution = (nums[i], nums[left], nums[right])

        current = sum(solution)

        if current < target:
            left += 1
        elif current > target:
            right -= 1
        else:
            left += 1
            right -= 1

            if solution in seen:
                continue

            seen.add(solution)
            print(*solution)
