# functions

# least_sq
# pdf wrappers
# Probit


###################################################
######### Process Data
###################################################

function least_sq(X::Array,Y::Array;N=int(size(X,1)), W=1)
  l = minimum(size(X))
  A = X'*W*X
  if sum(size(A))== 1
    inv_term = 1./A
  else
    inv_term = A\eye(int(size(X,2)))
  end
  β = inv_term * X'*W*Y
  if l == 1
    sigma_hat = sqrt(sum((1/N).* (Y - (β*X')')'*(Y - (β*X')'  ) ) ) #sum converts to Float64
  else
    sigma_hat = sqrt(sum((1/N).* (Y - (X*β))'*(Y - (X*β)  ) ) ) #sum converts to Float64
  end
  VCV = (sigma_hat).^2 * inv_term * eye(l)
  return β, sigma_hat, VCV
end

function least_sq(X::DataArray,Y::DataArray;N=int(size(X,1)), W=1)
  l = minimum( [size(X,2),size(X,1)]) # b/c array has size 0
  X = convert(Array{Float64,l},X)
  Y = convert(Array{Float64,1},Y)
  A = X'*W*X
  if sum(size(A))== 1
    inv_term = 1./A
  else
    inv_term = A\eye(int(size(X,2)))
  end
  β = inv_term * X'*W*Y
  if l == 1
    sigma_hat = sqrt(sum((1/N).* (Y - (β*X')')'*(Y - (β*X')'  ) ) ) #sum converts to Float64
  else
    sigma_hat = sqrt(sum((1/N).* (Y - (X*β))'*(Y - (X*β)  ) ) ) #sum converts to Float64
  end
  VCV = (sigma_hat).^2 * inv_term * eye(l)
  return β, sigma_hat, VCV
end

###################################################
######### pdf wrappers
###################################################

## Normal PDF
function normpdf(x::Vector{Float64};mean=0,var=1)
	out = Distributions.pdf(Distributions.Normal(mean,var), x) 
end

function normpdf(x::Vector{Float64};mean=0,var=1)
	out = Distributions.pdf(Distributions.Normal(mean,var), x) 
end

function normpdf(x::DataArray;mean=0,var=1)
	out = Distributions.pdf(Distributions.Normal(mean,var), x) 
end

## Normal CDF
function normcdf(x::Vector{Float64};mean=0,var=1)
	out = Distributions.cdf(Distributions.Normal(mean,var), x)
	out + (out .== 0.0)*eps(1.0) - (out .== 1.0)*eps(1.0) 
end

function normcdf(x::Vector{Float64};mean=0,var=1)
	out = Distributions.cdf(Distributions.Normal(mean,var), x) 
	out + (out .== 0.0)*eps(1.0) - (out .== 1.0)*eps(1.0)
end

function normcdf(x::DataArray;mean=0,var=1)
	out = Distributions.cdf(Distributions.Normal(mean,var), x) 
	out + (out .== 0.0)*eps(1.0) - (out .== 1.0)*eps(1.0)
end

###################################################
######### Probit
###################################################


function λ(θ::Vector{Float64})
	q =	2d-1
	q .* normpdf(q .* X*θ) ./ normcdf(q.*X*θ)
end


function probit_LL(θ::Vector{Float64})
	out = - sum( log( normcdf( (2d-1) .* X*θ) ) )
end

function probit_LL_g(θ::Vector{Float64}, grad::Vector{Float64})
	out = - sum( log( normcdf( (2d-1) .* X*θ) ) )
	if length(grad) > 0
		grad[:] = - sum( λ(θ) .* X,  1 )
	end
	out
end

function probit_gradient!(θ::Vector{Float64}, grad::Vector{Float64})
  grad[:] = - sum( λ(θ) .* X,  1 )
end

function probit_hessian!(θ::Vector{Float64}, hessian::Matrix{Float64})
  hh = zeros(size(hessian))
  A = λ(θ) .* ( λ(θ) + X*θ )
  for i in 1:size(X)[1]
    hh += A[i] * X[i,:]'*X[i,:]
  end
  hessian[:] = hh
end

function probit_vcov_score(θ::Vector{Float64}, g!)
	K    = length(θ)
	N = maximum(size(X))
	score - zeros(K,1)
	g!(θ,score)
	vcv_hessian = N*(score*score') \ eye(K)
end

function probit_vcov_hessian(θ::Vector{Float64}, h!)
	K    = length(θ)
	hessian = zeros((K,K))
	h!(θ, hessian)
	vcv_hessian = N*(hessian\eye(K))
end

function probit_results(θ::Vector,g!,h!)

	K = length(θ)
	
	vcv_hessian = repmat([NaN],K,K)
	try
		vcv_hessian = probit_vcov_hessian(θ, h!)
	catch
		println("No hessian for probit")
	end
	vcv_score = repmat([NaN],K,K)	
	try 
		vcv_score   = probit_vcov_score(θ, g!)
	catch
		println("No outer product for probit")
	end

	std_h = sqrt(diag(vcv_hessian))
	std_s = sqrt(diag(vcv_score))
	
	z_stat = θ./std_h
	pvals = Distributions.cdf(Distributions.Normal(), -abs(z_stat))

	X_bar = mean(X,1)
	# # Partial Effect at the Average
	ME1 = normpdf(vec(X_bar'.*θ)) .* θ
	# # Average Partial Effect (pg. 5)
	ME2 = mean(  normpdf( vec(X*θ) )  ) * θ

	return [
	 "θ"=>θ ,
	 "std_hess" => std_h,"std_score" => std_s,
	 "vcv_hessian" => vcv_hessian, "vcv_score" => vcv_score,
	 "z_stat"=> z_stat, "pvals"=> pvals,
	 "ME1"=>ME1,"ME2"=>ME2]
end


function unpackparams(θ::Vector{Float64})
  d = minimum(size(θ))
  θ = squeeze(θ,d)
  δ_0 = θ[1]
  δ_1 = θ[2]
  β_0 = θ[3]
  β_1 = θ[4]
  γ_0 = θ[5]
  γ_2 = θ[6]
  γ_3 = θ[7]
  α_0 = θ[8]
  α_1 = θ[9]
  α_C = θ[10]
  σ_C = θ[11]
  σ_1 = θ[12]
  σ_2 = θ[13]
  β_A = θ[14]
  σ_A = θ[15]
  β_B = θ[16]
  σ_B = θ[17]
  σ_θ = θ[18]

  return [  "δ_0" => δ_0,
  "δ_1" => δ_1,
  "β_0" => β_0,
  "β_1" => β_1,
  "γ_0" => γ_0,
  "γ_2" => γ_2,
  "γ_3" => γ_3,
  "α_0" => α_0,
  "α_1" => α_1,
  "α_C" => α_C,
  "σ_C" => σ_C,
  "σ_1" => σ_1,
  "σ_2" => σ_2,
  "β_A" => β_A,
  "σ_A" => σ_A,
  "β_B" => β_B,
  "σ_B" => σ_B,
  "σ_θ" => σ_θ]
end
