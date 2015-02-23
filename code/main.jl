using DataFrames
using Distributions
using Optim

###################################################
######### Structure of Code
###################################################
# Process data
# OLS
# Run probit
  # Probit value function
  # Probit gradient
  # Probit hessian
# Process Probit results
# recover structural parameters
# estimate variance of structural parameters
# EM algorithm

###################################################
######### Basic Parameters
###################################################
data_dir = "C:/Users/Nick/SkyDrive/One_data/LaborEcon/PS7/"
cd(data_dir)
fs = "data_ps7_spring2015.raw"
namevec = [symbol("id"),symbol("S"),symbol("Y"),symbol("M_a"),symbol("M_b"),symbol("X"),symbol("Z"),symbol("X_m")]

data = readtable(fs, separator = ' ', header = true, names = namevec)



code_dir = "C:/Users/Nick/SkyDrive/One_data/LaborEcon/PS7/code"
cd(code_dir)
include("functions.jl")

###################################################
######### Read in data. Use DataFrames
###################################################


###################################################
######### Process Data
###################################################

# TODO flip data to make it (obs x var)

N = int(size(data,1))
N_1 = sum(data[:S])
N_0 = N - N_1

# create constant
data[:C] = vec(ones(N,1))

data[:Y_0] = NaN
data[:Y_1] = NaN
data[data[:S] .== 1,:Y_0]  = data[data[:S] .== 1,:Y]
data[data[:S] .== 0,:Y_1]  = data[data[:S] .== 0,:Y]

sel0 = data[:S] .== 0
sel1 = data[:S] .== 1

K_A       = 2
K_B       = 2
numparams = 10 # just a guess

###################################################
######### Step 1
###################################################

# notice that they all have the same mean
# mean(data[:M_a])
# mean(data[:M_b])
# mean(data[:X_m])


# OLS on measurement equations
(β_A,σ_a,VCV_a) = least_sq(data[:X_m],data[:M_a])
se_β_A = sqrt(VCV_a)

(β_B,σ_b,VCV_b) = least_sq(data[:X_m],data[:M_b])
se_β_B = sqrt(VCV_b)

# TODO publish params

###################################################
######### Heckman 2-step
###################################################

# Step 1: Probit
# probit data
X = [vec(data[:C])  vec(data[:X]) vec(data[:Z]) ]
d = convert(Array,data[:S]) 

# optimization
iters = 1
f = probit_LL
g! = probit_gradient!
h! = probit_hessian!

initials = squeeze( (X'X)\X'd, 2).*2.*rand(size(X,2))

probit_opt = []
for kk = 1:iters
  probit_opt = Optim.optimize(f,g!,h!,vec(initials),
    xtol = 1e-32,
    ftol = 1e-32,
    grtol = 1e-14,
    iterations = 3000)
  initials = probit_opt.minimum
end
probit_opt

probit_res        = probit_results(probit_opt.minimum,g!,h!)
param_probit      = probit_res["θ"]
param_probit_se   = probit_res["std_hess"]
VCV_probit        = probit_res["vcv_hessian"]
param_probit_z    = probit_res["z_stat"]
param_probit_pval = probit_res["pvals"]
# probit_res["ME1"]
# probit_res["ME2"]

# Step 2: OLS

t = -(X*param_probit)
λ_0 = -normpdf(t)./normcdf(t)


# Y_0
Y0 = convert(Array,data[sel0,:Y])
X0 = [vec(data[sel0,:C]) vec(data[sel0,:X]) λ_0[sel0] ]

(ρ_0,~,VCV_ρ_0) = least_sq(X0,Y0)
se_ρ_0 = sqrt(diag(VCV_ρ_0))

# Y_1
Y1 = convert(Array,data[sel1,:Y])
X1 = [vec(data[sel1,:C]) vec(data[sel1,:X]) λ_0[sel1] ]

(ρ_1,~,VCV_ρ_1) = least_sq(X1,Y1)
se_ρ_1 = sqrt(diag(VCV_ρ_1))

# publish params

δ_0    = ρ_0[1]
β_0    = ρ_0[2]
π_0    = ρ_0[3]
δ_0_se = se_ρ_0[1]
β_0_se = se_ρ_0[2]
π_0_se = se_ρ_0[3]

δ_1    = ρ_1[1]
β_1    = ρ_1[2]
π_1    = ρ_1[3]
δ_1_se = se_ρ_1[1]
β_1_se = se_ρ_1[2]
π_1_se = se_ρ_1[3]

###################################################
######### Recover some more parameters
###################################################

# cannot get gammas? Need them for next step. Estimate of ̂I



###################################################
######### Use covariances
###################################################

Y_0_Xβ = convert(Array,data[sel0,:Y]) 
    - [vec(data[sel0,:C]) vec(data[sel0,:X])]*[δ_0; β_0] 
Y_1_Xβ = convert(Array,data[sel1,:Y]) 
    - [vec(data[sel1,:C]) vec(data[sel1,:X])]*[δ_1; β_1]
M_A0_Xβ = convert(Array,data[sel0,:M_a]) 
    - vec(data[sel0,:X_m]).*β_A 
M_B0_Xβ = convert(Array,data[sel0,:M_b]) 
    - vec(data[sel0,:X_m]).*β_B 
M_A1_Xβ = convert(Array,data[sel1,:M_a]) 
    - vec(data[sel1,:X_m]).*β_A 
M_B1_Xβ = convert(Array,data[sel1,:M_b]) 
    - vec(data[sel1,:X_m]).*β_B 

cov_0_A = (1/N_0)*sum(Y_0_Xβ'*M_A0_Xβ)
cov_0_B = (1/N_0)*sum(Y_0_Xβ'*M_B0_Xβ)
cov_1_A = (1/N_1)*sum(Y_1_Xβ'*M_A1_Xβ)
cov_1_B = (1/N_1)*sum(Y_1_Xβ'*M_A1_Xβ)

###################################################
######### EM algorithm
###################################################


include("HG_wts.jl")

σ_θ = 1
initials = ones(18)
initials[1:4] = [ρ_0[1] ρ_1[1] ρ_0[2] ρ_1[2]]
opt_out = []

# Is the idea we try for 100 iterations b/w inner MLE and outer σ_θ optimization?
# what about a loop w/ "while (abs( opt_out.f_minimum - opt_out_old.f_minimum  ) > ftol) || (count < maxit) " ?
for i = 1:100
  count = 0

  opt_out = Optim.optimize(wtd_LL,vec(initials),
      xtol = 1e-32,
      ftol = 1e-32,
      grtol = 1e-14,
      iterations = 2000,
      autodiff=true)
  initials = opt_out.minimum

  update = unpackparams(opt_out.minimum)
  δ_0 = update["δ_0"]
  δ_1 = update["δ_1"]
  β_0 = update["β_0"]
  β_1 = update["β_1"]
  α_0 = update["α_0"]
  α_1 = update["α_1"]
  α_C = update["α_C"]
  β_A = update["β_A"]
  α_B = update["α_B"]
  β_B = update["β_B"]

  Y0        = convert(Array,data[sel0,:Y])
  X0        = [vec(data[sel0,:C]) vec(data[sel0,:X])]
  Y1        = convert(Array,data[sel1,:Y])
  X1        = [vec(data[sel1,:C]) vec(data[sel1,:X])]

  # Why are we getting a θ_hat? Is this to get an estimate for σ_θ?
  θ_hat = zeros(N)
  θ_A = data[:M_a]  - data[:X_m] .* β_A
  θ_B = (data[:M_b] - data[:X_m] .* β_B)./α_B
  θ_hat[sel1] = (1/3).* ( θ_A[sel1] + θ_B[sel1] +
          ( (Y1 - X1*[δ_1; β_1])  )./α_1 )
  θ_hat[sel0] = (1/3).* ( θ_A[sel0] + θ_B[sel0] +
          ( (Y0 - X0*[δ_0; β_0])  )./α_0 )
  σ_θ = var(θ_hat)
end


opt_out.minimum

ρ_0
ρ


str = ["δ_0", "δ_1", "β_0","β_1",
    "γ_0","γ_2","γ_3","α_0","α_1",
    "α_C","σ_C","σ_1","σ_2","β_A",
    "α_B","σ_A","β_B","σ_B"]

numparams = length(opt_out.minimum)
for i = 1:numparams

  @sprintf("%s  :  %5.3f ", [str[i] opt_out.minimum[i]])

end

# println("Coefficients from model 1: ")
# println("           [β_0,β_1,β_2] = $(round(beta_MLE,3))")
# println("  [SE(β0),SE(β1),SE(β1)] = $(round(beta_SE,3))")
# println("Coefficients from model 2:")
# println("              [γ0,γ1,γ2] = $(round(gamma,3))")
# println("  [SE(γ0),SE(γ1),SE(γ1)] = $(round(gamma_SE,3))")
# println(" ")
# println(" ")
# println("                     ρ is: $(round(rho_mle,3))")
# println("                 SE(ρ) is: $(round(rho_SE,3))")
# println("               Sigma_v is: $(round(sigma_v,3))")
# println("")
# println("            MAX Log(L) is: $(round(probit_opt.f_minimum,3))")