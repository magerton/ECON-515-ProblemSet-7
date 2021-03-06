using DataFrames
using Distributions
using Optim
using Dates
using Roots

doLog = false
originalSTDOUT = STDOUT

if doLog == true 
  (outRead, outWrite) = redirect_stdout()
  println("-------------------------------------------------------------------")
  println("Results for Homework 5\nStarted run at " * string(now()))
  println("-------------------------------------------------------------------")
end

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
dir = "C:/Users/Nick/SkyDrive/One_data/LaborEcon/PS7/"
# dir = "C:/mja3/SkyDrive/Rice/Class/econ515-Labor/PS7/"
fs  = "data_ps7_spring2015.raw"

cd(dir)
namevec = [symbol("id"),symbol("S"),symbol("Y"),symbol("M_a"),symbol("M_b"),symbol("X"),symbol("Z"),symbol("X_m")]
data    = readtable(fs, separator = ' ', header = true, names = namevec)

cd("./code/")
include("./functions.jl")

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
λ_1 = normpdf(t)./(1-normcdf(t))

# Y_0
Y0 = convert(Array,data[sel0,:Y])
X0 = [vec(data[sel0,:C]) vec(data[sel0,:X]) λ_0[sel0] ]

(ρ_0,~,VCV_ρ_0) = least_sq(X0,Y0)
se_ρ_0 = sqrt(diag(VCV_ρ_0))

# Y_1
Y1 = convert(Array,data[sel1,:Y])
X1 = [vec(data[sel1,:C]) vec(data[sel1,:X]) λ_1[sel1] ]

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

println("Two-step parameters
     δ_0    = $(round( ρ_0[1]   , 2 )) 
     β_0    = $(round( ρ_0[2]   , 2 )) 
     π_0    = $(round( ρ_0[3]   , 2 )) 

     δ_0_se = $(round( se_ρ_0[1], 2 )) 
     β_0_se = $(round( se_ρ_0[2], 2 )) 
     π_0_se = $(round( se_ρ_0[3], 2 )) 

     δ_1    = $(round( ρ_1[1]   , 2 )) 
     β_1    = $(round( ρ_1[2]   , 2 )) 
     π_1    = $(round( ρ_1[3]   , 2 )) 

     δ_1_se = $(round( se_ρ_1[1], 2 )) 
     β_1_se = $(round( se_ρ_1[2], 2 )) 
     π_1_se = $(round( se_ρ_1[3], 2 )) \n " )

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
M_A_Xβ = convert(Array,data[:M_a]) 
    - vec(data[:X_m]).*β_A 
M_B_Xβ = convert(Array,data[:M_b]) 
    - vec(data[:X_m]).*β_B 

cov_0_A = (1/N_0)*sum(Y_0_Xβ'*M_A0_Xβ)
cov_0_B = (1/N_0)*sum(Y_0_Xβ'*M_B0_Xβ)
cov_1_A = (1/N_1)*sum(Y_1_Xβ'*M_A1_Xβ)
cov_1_B = (1/N_1)*sum(Y_1_Xβ'*M_A1_Xβ)
cov_A_B = (1/N)*sum(M_A_Xβ'*M_B_Xβ)

α_B = cov_0_B/cov_0_A
σ_θ = sqrt(cov_A_B/α_B)
α_0 = cov_0_A/(σ_θ^2)
α_1 = cov_1_A/(σ_θ^2)

# σ_A = sqrt( var(convert(Array,data[:M_a])) - σ_θ^2  )
#########################################################
σ_A_sq =  var(convert(Array,data[:M_a])) - σ_θ^2  ### < NEGATIVE VARIANCE
σ_B = sqrt(  var(convert(Array,data[:M_b])) -α_B^2*σ_θ^2  )

ρ_ηθ_0 = π_0 / (α_0*σ_θ)
ρ_ηθ_1 = π_1 / (α_1*σ_θ)
########################################################
ρ_ηθ   = ρ_ηθ_0 # ρ_ηθ_1  #### < NOT THE SAME NUMEBR

δ_t_0 = λ_0[sel0]'*(λ_0[sel0] - t[sel0])
σ_0_ρ0 = sqrt( 
    var(convert(Array,data[sel0,:Y])) - α_0^2.*(ρ_ηθ_0*σ_θ)^2*
    (1 -δ_t_0) - σ_θ^2*(1-ρ_ηθ_0^2)
  ) # variance on income. Large
σ_0_ρ1 = sqrt( 
    var(convert(Array,data[sel0,:Y])) - α_0^2.*(ρ_ηθ_1*σ_θ)^2*
    (1 -δ_t_0) - σ_θ^2*(1-ρ_ηθ_1^2)
  ) # variance on income. Large


δ_t_1 = λ_1[sel1]'*(λ_1[sel1] - t[sel1])
σ_1_ρ0 = sqrt( 
    var(convert(Array,data[sel1,:Y])) - α_1^2.*(ρ_ηθ_0*σ_θ)^2*
    (1 -δ_t_1) - σ_θ^2*(1-ρ_ηθ_0^2)
  ) # variance on income. Large
σ_1_ρ1 = sqrt( 
    var(convert(Array,data[sel1,:Y])) - α_1^2.*(ρ_ηθ_1*σ_θ)^2*
    (1 -δ_t_1) - σ_θ^2*(1-ρ_ηθ_1^2)
  ) # variance on income. Large


# δ_t_0 = - λ_0[sel0].*t[sel0] + λ_0[sel0].*λ_0[sel0] # causes negative number
# σ_0 = sqrt(
#   (1/N_0)*sum( 
#     var(convert(Array,data[sel0,:Y])) - α_0^2.*(ρ_ηθ*σ_θ)^2*
#     (1 - δ_t_0) - σ_θ^2*(1-ρ_ηθ^2) )
#   ) # variance on income. Large

# δ_t_1 = - λ_1[sel1].*t[sel1] + λ_0[sel1].*λ_1[sel1]
# σ_1 = sqrt( 
#     (1/N_1)*sum (
#     var(convert(Array,data[sel1,:Y])) - α_1^2.*(ρ_ηθ*σ_θ)^2*
#     (1 -δ_t_1) - σ_θ^2*(1-ρ_ηθ^2) )
#   ) # variance on income. Large

f_c_ρ0(α_c) = ρ_ηθ_0*σ_θ - (α_1 - α_0 - α_c)*σ_θ^2*(
  (α_1 - α_0 - α_c)^2*σ_θ^2 + 1)^(-.5)
f_c_ρ1(α_c) = ρ_ηθ_1*σ_θ - (α_1 - α_0 - α_c)*σ_θ^2*(
  (α_1 - α_0 - α_c)^2*σ_θ^2 + 1)^(-.5)
α_c_ρ0 = fzero(f_c_ρ0, -1000, 1000)
α_c_ρ1 = fzero(f_c_ρ1, -1000, 1000)

# normalize σ_c =1
σ_c = 1

σ_star = sqrt( (α_1 - α_0 - α_c)^2*σ_θ^2 + σ_c^2)

γ_0 = δ_1 - δ_0 - param_probit[1]*σ_star
γ_1 = β_1 - β_0 - param_probit[2]*σ_star
γ_3 = param_probit[3]*σ_star

# these are wrong b/c ignore variance of σ_star
# (b/c it looks hard)
V(kk) =[VCV_ρ_1[kk]     0           0;
    0         VCV_ρ_0[kk]       0;
    0              0     VCV_probit[kk]]
γ_0_se = [1 -1 -1]*V(1)*[1 -1 -1]'
γ_1_se = [1 -1 -1]*V(2)*[1 -1 -1]'
γ_3_se = [0  0  1]*V(3)*[0  0  1]'



println("Cov from two-step
    cov_0_A = $(round(cov_0_A, 2))
    cov_0_B = $(round(cov_0_B, 2))
    cov_1_A = $(round(cov_1_A, 2))
    cov_1_B = $(round(cov_1_B, 2)) 
    cov_A_B = $(round(cov_A_B, 2)) \n")


println("Final Parameters
  α_B    = $(round(α_B   ,3))
  σ_θ    = $(round(σ_θ   ,3))
  α_1    = $(round(α_1   ,3))
  α_0    = $(round(α_0   ,3))
  σ_A_sq = $(round(NaN   ,3))   squared: $(round(σ_A_sq,3)) 
  σ_B    = $(round(σ_B   ,3)) squared: $(round(σ_B.^2,3))

  ρ_ηθ   = $(round(ρ_ηθ_0  ,3)) # using ρ_ηθ_0 

    σ_0    = $(round(σ_0_ρ0   ,3)) squared: $(round(σ_0.^2,3))
    σ_1    = $(round(σ_1_ρ0   ,3)) squared: $(round(σ_1.^2,3))
    α_c    = $(round(α_c_ρ0   ,3))

  ρ_ηθ   = $(round(ρ_ηθ_1  ,3)) # using ρ_ηθ_1

    σ_0    = $(round(σ_0_ρ1   ,3)) squared: $(round(σ_0.^2,3))
    σ_1    = $(round(σ_1_ρ1   ,3)) squared: $(round(σ_1.^2,3))
    α_c    = $(round(α_c_ρ1   ,3))

  With normalization σ_c =1:
  σ_c    = $(round(σ_c   ,3)) # normalize 
    γ_0    = $(round(γ_0   ,3))
    γ_1    = $(round(γ_1   ,3))
    γ_3    = $(round(γ_3   ,3))

  Incidental parameters
  σ_star = $(round(σ_star,3))")




# ###################################################
# ######### EM algorithm
# ###################################################


# include("./HG_wts.jl")

# σ_θ = 1
# initials = ones(18)
# initials[1:4] = [ρ_0[1] ρ_1[1] ρ_0[2] ρ_1[2]]
# opt_out = []

# println("Doing EM!\n")

# # Loop w/ "while (abs( opt_out.f_minimum - opt_out_old.f_minimum  ) > ftol) || (count < maxit) " ?
# for i = 1:5
  
#   global count = 0
#   println("\n ----------Update $i: σ_θ = $(σ_θ) --------------\n\n")
  
#   opt_out = Optim.optimize(wtd_LL,vec(initials),
#       xtol = 1e-32,
#       ftol = 1e-32,
#       grtol = 1e-14,
#       iterations = 500,
#       autodiff=true)

#   initials = opt_out.minimum

#   println("\nResults: \t $opt_out \n\n")

#   update = unpackparams(opt_out.minimum)
#   δ_0 = update["δ_0"]
#   δ_1 = update["δ_1"]
#   β_0 = update["β_0"]
#   β_1 = update["β_1"]
#   α_0 = update["α_0"]
#   α_1 = update["α_1"]
#   α_C = update["α_C"]
#   β_A = update["β_A"]
#   α_B = update["α_B"]
#   β_B = update["β_B"]

#   Y0        = convert(Array,data[sel0,:Y])
#   X0        = [vec(data[sel0,:C]) vec(data[sel0,:X])]
#   Y1        = convert(Array,data[sel1,:Y])
#   X1        = [vec(data[sel1,:C]) vec(data[sel1,:X])]

#   # Form an updated estimate for θ_hat
#   θ_hat = zeros(N)
#   θ_A = data[:M_a]  - data[:X_m] .* β_A
#   θ_B = (data[:M_b] - data[:X_m] .* β_B)./α_B
#   θ_hat[sel1] = (1/3).* ( θ_A[sel1] + θ_B[sel1] +
#           ( (Y1 - X1*[δ_1; β_1])  )./α_1 )
#   θ_hat[sel0] = (1/3).* ( θ_A[sel0] + θ_B[sel0] +
#           ( (Y0 - X0*[δ_0; β_0])  )./α_0 )
#   σ_θ = sqrt(var(θ_hat))

# println("\t
#   δ_0 = $(round(opt_out.minimum[1]  , 2))
#   δ_1 = $(round(opt_out.minimum[2]  , 2))
#   β_0 = $(round(opt_out.minimum[3]  , 2))
#   β_1 = $(round(opt_out.minimum[4]  , 2))

#   γ_0 = $(round(opt_out.minimum[5]  , 2))
#   γ_2 = $(round(opt_out.minimum[6]  , 2))
#   γ_3 = $(round(opt_out.minimum[7]  , 2))

#   α_0 = $(round(opt_out.minimum[8]  , 2))
#   α_1 = $(round(opt_out.minimum[9]  , 2))
#   α_C = $(round(opt_out.minimum[10] , 2))

#   σ_C = $(round(opt_out.minimum[11] , 2))
#   σ_1 = $(round(opt_out.minimum[12] , 2))
#   σ_2 = $(round(opt_out.minimum[13] , 2))

#   α_A = 1 (normalized)
#   β_A = $(round(opt_out.minimum[14] , 2))
#   σ_A = $(round(opt_out.minimum[15] , 2))
#   α_B = $(round(opt_out.minimum[16] , 2))
#   β_B = $(round(opt_out.minimum[17] , 2))
#   σ_B = $(round(opt_out.minimum[18] , 2))")

# end


if doLog == true

  println("-------------------------------------------------------------------")
  println("Finished run at " * string(now()))
  println("-------------------------------------------------------------------")

  close(outWrite)
  stringOut = readavailable(outRead)
  close(outRead)
  redirect_stdout(originalSTDOUT)

  f = open("../Hwk7-Results.txt", "w")
  write(f, stringOut )
  close(f)

  println(stringOut)
end