# Fiecare student primește un i unic
i <- 1

# Exercițiul 1
# Folosim repartiția hipergeometrică
p1 <- dhyper(x=1, m=2, n=i, k=2)
p2 <- dhyper(x=2, m=2, n=i, k=2)

print("P(X = 2):")
print(p1 / (p1 + p2))

# Exercițiul 2
# Folosim repartiția binomială
print("P(X = i):")
print(dbinom(i, size=100, prob=1/6))

# Exercițiul 3
# Folosim repartiția normală
right <- 1

print("P(X < 1):")
print(pnorm(right, mean=3, sd=sqrt(54)))

# Statistică
X <- c(1, 2, 3, i + 3)
n <- 4

# Exercițiul 4
X_bar <- mean(X)
sigma <- sqrt(3)

alpha <- 0.01

dx <- (sigma*qnorm(1 - alpha/2))/sqrt(n)

c(X_bar - dx, X_bar + dx)

# Exercițiul 5
s = var(X)

aux <- (n - 1) * s

left <- aux/qchisq(alpha/2, df=n-1)
right <- aux/qchisq(1-alpha/2, df=n-1)

# Exercițiul 6
C <- cbind(1, X, X^2)
y <- c(1, 2, 3, 4)

# beta = (C^T * C)^(-1) * C^T * y
beta <- solve(t(C)%*%C, t(C)%*%y)

print(t(beta)) # Afișare pe linie

# Verificare cu lm
X_sq <- X^2

model <- lm(y ~ X + X_sq)
print(model$coefficients)

