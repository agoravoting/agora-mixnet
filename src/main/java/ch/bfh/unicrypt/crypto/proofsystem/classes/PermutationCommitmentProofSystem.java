/*
 * UniCrypt
 *
 *  UniCrypt(tm) : Cryptographical framework allowing the implementation of cryptographic protocols e.g. e-voting
 *  Copyright (C) 2014 Bern University of Applied Sciences (BFH), Research Institute for
 *  Security in the Information Society (RISIS), E-Voting Group (EVG)
 *  Quellgasse 21, CH-2501 Biel, Switzerland
 *
 *  Licensed under Dual License consisting of:
 *  1. GNU Affero General Public License (AGPL) v3
 *  and
 *  2. Commercial license
 *
 *
 *  1. This program is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU Affero General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU Affero General Public License for more details.
 *
 *   You should have received a copy of the GNU Affero General Public License
 *   along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 *  2. Licensees holding valid commercial licenses for UniCrypt may use this file in
 *   accordance with the commercial license agreement provided with the
 *   Software or, alternatively, in accordance with the terms contained in
 *   a written agreement between you and Bern University of Applied Sciences (BFH), Research Institute for
 *   Security in the Information Society (RISIS), E-Voting Group (EVG)
 *   Quellgasse 21, CH-2501 Biel, Switzerland.
 *
 *
 *   For further information contact <e-mail: unicrypt@bfh.ch>
 *
 *
 * Redistributions of files must retain the above copyright notice.
 */
package ch.bfh.unicrypt.crypto.proofsystem.classes;

import ch.bfh.unicrypt.crypto.proofsystem.abstracts.AbstractProofSystem;
import ch.bfh.unicrypt.crypto.proofsystem.challengegenerator.classes.RandomOracleChallengeGenerator;
import ch.bfh.unicrypt.crypto.proofsystem.challengegenerator.classes.RandomOracleSigmaChallengeGenerator;
import ch.bfh.unicrypt.crypto.proofsystem.challengegenerator.interfaces.ChallengeGenerator;
import ch.bfh.unicrypt.crypto.proofsystem.challengegenerator.interfaces.SigmaChallengeGenerator;
import ch.bfh.unicrypt.crypto.schemes.commitment.classes.GeneralizedPedersenCommitmentScheme;
import ch.bfh.unicrypt.helper.math.MathUtil;
import ch.bfh.unicrypt.helper.random.RandomByteSequence;
import ch.bfh.unicrypt.helper.random.RandomOracle;
import ch.bfh.unicrypt.helper.random.deterministic.DeterministicRandomByteSequence;
import ch.bfh.unicrypt.math.algebra.dualistic.classes.ZMod;
import ch.bfh.unicrypt.math.algebra.dualistic.classes.ZModElement;
import ch.bfh.unicrypt.math.algebra.general.classes.Pair;
import ch.bfh.unicrypt.math.algebra.general.classes.PermutationElement;
import ch.bfh.unicrypt.math.algebra.general.classes.PermutationGroup;
import ch.bfh.unicrypt.math.algebra.general.classes.ProductGroup;
import ch.bfh.unicrypt.math.algebra.general.classes.ProductSet;
import ch.bfh.unicrypt.math.algebra.general.classes.Triple;
import ch.bfh.unicrypt.math.algebra.general.classes.Tuple;
import ch.bfh.unicrypt.math.algebra.general.interfaces.CyclicGroup;
import ch.bfh.unicrypt.math.algebra.general.interfaces.Element;
import ch.bfh.unicrypt.math.algebra.general.interfaces.Group;
import ch.bfh.unicrypt.math.algebra.general.interfaces.Set;
import ch.bfh.unicrypt.math.function.abstracts.AbstractFunction;
import ch.bfh.unicrypt.math.function.classes.ConvertFunction;
import ch.bfh.unicrypt.math.function.classes.PermutationFunction;
import ch.bfh.unicrypt.math.function.classes.ProductFunction;


import ch.bfh.unicrypt.math.algebra.general.abstracts.AbstractCyclicGroup;
import mpservice.MPBridge;

//
// @see [TW10] Protocol 1: Permutation Matrix
//
public class PermutationCommitmentProofSystem
	   extends AbstractProofSystem<ProductGroup, Pair, ProductGroup, Tuple, ProductSet, Tuple> {

	final private static int DEFAULT_KR = 20;

	final private SigmaChallengeGenerator sigmaChallengeGenerator;
	final private ChallengeGenerator eValuesGenerator;
	final private CyclicGroup cyclicGroup;
	final private int size;
	final private int ke;
	final private int kc;
	final private int kr;
	final private Tuple independentGenerators;

	private PermutationCommitmentProofSystem(SigmaChallengeGenerator sigmaChallengeGenerator,
		   ChallengeGenerator eValuesGenerator,
		   CyclicGroup cyclicGroup, int size, int kr, Tuple independentGenerators) {

		this.sigmaChallengeGenerator = sigmaChallengeGenerator;
		this.eValuesGenerator = eValuesGenerator;
		this.cyclicGroup = cyclicGroup;
		this.size = size;
		this.kr = kr;
		this.independentGenerators = independentGenerators;

		this.ke = ((ZMod) ((ProductSet) this.eValuesGenerator.getChallengeSpace()).getFirst()).getModulus()
			   .subtract(MathUtil.ONE).bitLength();
		this.kc = this.sigmaChallengeGenerator.getChallengeSpace().getModulus().subtract(MathUtil.ONE).bitLength();
	}

	//===================================================================================
	// Interface implementation
	//
	// Private: (PermutationElement pi, PermutationCommitment-Randomizations sV)
	@Override
	protected ProductGroup abstractGetPrivateInputSpace() {
		return ProductGroup.getInstance(PermutationGroup.getInstance(this.size),
										ProductGroup.getInstance(this.cyclicGroup.getZModOrder(), this.size));
	}

	// Public:  (PermutationCommitment cPiV)
	@Override
	protected ProductGroup abstractGetPublicInputSpace() {
		return ProductGroup.getInstance(this.cyclicGroup, this.size);
	}

	// Proof:   (e-Values, Bridging Commitments, t, c, s)
	@Override
	protected ProductSet abstractGetProofSpace() {
		return ProductSet.getInstance(this.eValuesGenerator.getChallengeSpace(),
									  ProductGroup.getInstance(this.cyclicGroup, this.size),
									  this.getCommitmentSpace(),
									  this.getChallengeSpace(),
									  this.getResponseSpace());
	}

	// t: G_q^(N+3)
	public ProductGroup getCommitmentSpace() {
		return ProductGroup.getInstance(this.cyclicGroup, this.size + 3);
	}

	// c: [0,...,2^kc - 1]
	public ZMod getChallengeSpace() {
		return this.sigmaChallengeGenerator.getChallengeSpace();
	}

	// s: (v, w, rV, d, eV)
	public ProductGroup getResponseSpace() {
		return ProductGroup.getInstance(cyclicGroup.getZModOrder(),
										cyclicGroup.getZModOrder(),
										ProductGroup.getInstance(cyclicGroup.getZModOrder(), size),
										cyclicGroup.getZModOrder(),
										ProductGroup.getInstance(cyclicGroup.getZModOrder(), size));
	}

	public Element getEValues(final Tuple proof) {
		if (!this.getProofSpace().contains(proof)) {
			throw new IllegalArgumentException();
		}
		return proof.getAt(0);
	}

	public Element getBridingCommitment(final Tuple proof) {
		if (!this.getProofSpace().contains(proof)) {
			throw new IllegalArgumentException();
		}
		return proof.getAt(1);
	}

	public Element getCommitment(final Tuple proof) {
		if (!this.getProofSpace().contains(proof)) {
			throw new IllegalArgumentException();
		}
		return proof.getAt(2);
	}

	public Element getChallenge(final Tuple proof) {
		if (!this.getProofSpace().contains(proof)) {
			throw new IllegalArgumentException();
		}
		return proof.getAt(3);
	}

	public Element getResponse(final Tuple proof) {
		if (!this.getProofSpace().contains(proof)) {
			throw new IllegalArgumentException();
		}
		return proof.getAt(4);
	}

	//===================================================================================
	// Generate and Validate
	//
	@Override
	protected Tuple abstractGenerate(Pair privateInput, Tuple publicInput, RandomByteSequence randomByteSequence) {

		// Unfold private and public input
		final PermutationElement pi = (PermutationElement) privateInput.getFirst();
		final Tuple sV = (Tuple) privateInput.getSecond();
		final Tuple eV = (Tuple) this.eValuesGenerator.generate(publicInput);

		// Compute private values for sigma proof
		// v = <1,sV> = sum(sV)
		final Element v = this.cyclicGroup.getZModOrder().add(sV);
		// w = <sV,eV>

		final Tuple rV
			   = ProductGroup.getInstance(this.cyclicGroup.getZModOrder(), this.size).getRandomElement(randomByteSequence);
		Tuple ePrimeV = PermutationFunction.getInstance(eV.getSet()).apply(eV, pi);

		// Compute commitments c_i and d
		final Element g = this.independentGenerators.getAt(0);
		final Element h = this.independentGenerators.getAt(1);

		final Element[] cs = new Element[this.size];
		final Element[] ds = new Element[this.size];
		ds[0] = rV.getAt(0);		
		
		/*MPBridge.a();
		for (int i = 0; i < this.size; i++) {
			Element c_i_1 = i == 0 ? h : cs[i - 1];
			cs[i] = g.selfApply(rV.getAt(i)).apply(c_i_1.selfApply(ePrimeV.getAt(i)));  //   [2n]
			if (i > 0) {
				ds[i] = rV.getAt(i).apply(ds[i - 1].selfApply(ePrimeV.getAt(i)));
			}
		}
		MPBridge.b();
		*/
		
		final Element[] temp = new Element[this.size];
		final Tuple ePrimeVFinal = ePrimeV;
		MPBridge.ex(() -> {
			for (int i = 0; i < this.size; i++) {	
				temp[i] = g.selfApply(rV.getAt(i));  //   [2n]
				if (i > 0) {
					ds[i] = rV.getAt(i).apply(ds[i - 1].selfApply(ePrimeVFinal.getAt(i)));
				}
			}
			return 0;
		}, "2");

		System.out.println("Cannot parallelize..");
		long now = System.currentTimeMillis();
		// CANT BE PARALLELIZED
		MPBridge.a();
		for (int i = 0; i < this.size; i++) {
			Element c_i_1 = i == 0 ? h : cs[i - 1];
			cs[i] = temp[i].apply(c_i_1.selfApply(ePrimeV.getAt(i)));  //   [2n]
		}
		MPBridge.b();
		System.out.println("Bad loop: " + ((System.currentTimeMillis() - now) / 1000.0));
		
		
		final Tuple cV = Tuple.getInstance(cs);
		final Element d = ds[ds.length - 1];

		// Map ePrimeV to Z_q^N
		ePrimeV = ProductFunction.getInstance(
			   ConvertFunction.getInstance(ePrimeV.getFirst().getSet(), cyclicGroup.getZModOrder()),
			   ePrimeV.getLength()).apply(ePrimeV);

		// Create sigma proof
		PreimageProofFunction f
			   = new PreimageProofFunction(this.cyclicGroup, this.size, this.getResponseSpace(),
										   this.getCommitmentSpace(), this.independentGenerators, cV);
		Tuple randomElement = this.getResponseSpace().extractPrefix(4).getRandomElement(randomByteSequence);
		Tuple randEV = ProductGroup.getInstance(ZMod.getInstance(MathUtil.powerOfTwo(this.ke + this.kc + this.kr)),
												this.size).getRandomElement(randomByteSequence);

		// 'Normally' ke+kc+kr < cyclicGroup.getOrder
		Element[] randEVs = new Element[this.size];
		for (int i = 0; i < randEVs.length; i++) {
			randEVs[i] = cyclicGroup.getZModOrder().getElement(randEV.getAt(i).convertToBigInteger().mod(cyclicGroup.getOrder()));
		}
		randEV = Tuple.getInstance(randEVs);
		randomElement = randomElement.append(Tuple.getInstance(randEV));

		///
		long before = MPBridge.total;
		final Element commitment = f.apply(randomElement);                              // [3n+3]
		System.out.println("perm gen f.apply " + (MPBridge.total - before));
		///
		
		final Element challenge = this.sigmaChallengeGenerator.generate(Pair.getInstance(publicInput, cV), commitment);
		final Element w = computeInnerProduct(sV, eV);
		
		final Element response = randomElement.apply(Tuple.getInstance(v, w, rV, d, ePrimeV).selfApply(challenge));
	
		Triple preimageProof = (Triple) Triple.getInstance(commitment, challenge, response);
		
		//                                                                                -------
		return Tuple.getInstance(eV, cV).append(preimageProof);                                     // [5n+3]
	}

	@Override
	protected boolean abstractVerify(Tuple proof, Tuple publicInput) {

		

		// Unfold proof
		final Tuple cV = (Tuple) proof.getAt(1);
		final Tuple commitment = (Tuple) proof.getAt(2);
		final Tuple response = (Tuple) proof.getAt(4);

		// Get additional values
		final Tuple eV = (Tuple) this.eValuesGenerator.generate(publicInput);
		final Tuple gV = this.independentGenerators.extract(1, this.size);


		// Compute image of preimage proof
		final Element[] ps = new Element[this.size + 3];
		// - p_0 = c_pi^1/prod(g_i) = prod(c_pi_i)/prod(g_i)
		ps[0] = this.cyclicGroup.apply(publicInput).applyInverse(this.cyclicGroup.apply(gV));
		

		///
		long before = MPBridge.total;
		// - p_1 = c_pi^e                                                                     [N]
		ps[1] = computeInnerProduct(publicInput, eV);
		System.out.println("perm ver computeInnerProduct " + (MPBridge.total - before));
		///
		
		// - p_2...p_(N+2) = c_1 ... c_N
		for (int i = 0; i < this.size; i++) {
			ps[i + 2] = cV.getAt(i);
		}
		
		
		
		// - p_(N+3) = c_N/h^(prod(e))                                                        [1]
		// Make sure prod(e) is computet in Z_q
		Element eProd = this.cyclicGroup.getZModOrder().getElement(eV.getAt(0).convertToBigInteger()
			   .mod(this.cyclicGroup.getOrder()));
		for (int i = 1; i < this.size; i++) {
			eProd = eProd.selfApply(eV.getAt(i));
		}
		
		ps[this.size + 2] = cV.getAt(this.size - 1).applyInverse(gV.getAt(0).selfApply(eProd));
		final Tuple pV = Tuple.getInstance(ps);


		// Verify preimage proof
		PreimageProofFunction f = new PreimageProofFunction(this.cyclicGroup,
															this.size, this.getResponseSpace(), this.getCommitmentSpace(), this.independentGenerators, cV);
		
		final Element challenge = this.sigmaChallengeGenerator.generate(Pair.getInstance(publicInput, cV), commitment);
		

		///
		before = MPBridge.total;
		final Element left = f.apply(response);                                         // [3N+3]
		System.out.println("perm ver f.apply " + (MPBridge.total - before));
		///
		
		///
		before = MPBridge.total;
		// MPBridge.debug = true;
		final Element right = commitment.apply(pV.selfApply(challenge));                //  [N+3]
		System.out.println("perm ver pv.selfApply " + (MPBridge.total - before));
		///

		//                                                                                -------
		return left.isEquivalent(right);                                                // [5N+7]
	}

	//===================================================================================
	// Private Helpers
	//
	// Helper to compute the inner product
	// - Additive:       Sum(t1_i*t2_i)
	// - Multiplicative: Prod(t1_i^(t2_i))
	private static Element computeInnerProduct(Tuple t1, Tuple t2) {
		if (!t1.getSet().isGroup() || t1.getArity() < 1) {
			throw new IllegalArgumentException();
		}
		Element innerProduct = ((Group) t1.getSet().getAt(0)).getIdentityElement();
		/*MPBridge.a();
		MPBridge.startRecord();
		for (int i = 0; i < t1.getArity(); i++) {
			innerProduct = innerProduct.apply(t1.getAt(i).selfApply(t2.getAt(i)));
		}
		mpservice.ModPow[] requests = MPBridge.stopRecord();
		MPBridge.b();
		if(requests.length > 0) {
			java.math.BigInteger[] answers = mpservice.MPService.compute(requests);
			MPBridge.startReplay(answers);
			innerProduct = ((Group) t1.getSet().getAt(0)).getIdentityElement();
			for (int i = 0; i < t1.getArity(); i++) {
				innerProduct = innerProduct.apply(t1.getAt(i).selfApply(t2.getAt(i)));
			}
			MPBridge.stopReplay();
		}
		MPBridge.reset();*/
		Element ret = MPBridge.ex(() -> {
			Element ip = innerProduct;
			for (int i = 0; i < t1.getArity(); i++) {
				ip = ip.apply(t1.getAt(i).selfApply(t2.getAt(i)));
			}
			return ip;
		}, "2");

		return ret;
		// return innerProduct;
	}

	//===================================================================================
	// Nested class PreimageProofFunction
	//
	private class PreimageProofFunction
		   extends AbstractFunction<PreimageProofFunction, ProductGroup, Tuple, ProductGroup, Tuple> {

		private final CyclicGroup cyclicGroup;
		private final int size;
		private final Tuple cV;
		private final GeneralizedPedersenCommitmentScheme gpcs;
		private final Element g;
		private final Element h;

		protected PreimageProofFunction(CyclicGroup cyclicGroup, int size, ProductGroup domain,
			   ProductGroup coDomain, Tuple independentGenerators, Tuple cV) {
			super(domain, coDomain);
			this.cyclicGroup = cyclicGroup;
			this.size = size;
			this.cV = cV;
			this.g = independentGenerators.getAt(0);
			this.h = independentGenerators.getAt(1);

			this.gpcs = GeneralizedPedersenCommitmentScheme.getInstance(g, independentGenerators.extract(1, size));
		}

		@Override
		protected Tuple abstractApply(Tuple element, RandomByteSequence randomByteSequence) {

			// Unfold element
			final Element v = element.getAt(0);
			final Element w = element.getAt(1);
			final Tuple rV = (Tuple) element.getAt(2);
			final Element d = element.getAt(3);
			final Tuple ePrimeV = (Tuple) element.getAt(4);

			// Result array
			final Element[] pV = new Element[this.size + 3];

			
			// COMPUTE...
			// - Com(0, v)                          [1]
			pV[0] = this.gpcs.getRandomizationGenerator().selfApply(v);

			
			
			// - Com(e', w)                       [n+1]
			Element ePrimeVs[] = new Element[ePrimeV.getArity()];
			ZMod zMod = this.cyclicGroup.getZModOrder();
			for (int i = 0; i < ePrimeV.getArity(); i++) {
				ePrimeVs[i] = zMod.getElement(((ZModElement) ePrimeV.getAt(i)).getValue().mod(zMod.getOrder()));
			}
			
			
			long before = MPBridge.total;
			pV[1] = this.gpcs.commit(Tuple.getInstance(ePrimeVs), w);
			System.out.println("perm preimageproof gpcs.commit " + (MPBridge.total - before));


			// MPBridge.a();
			// - g^r_i * c_i-1^e'_i                [2n]
			// for (int i = 0; i < this.size; i++) {
			//	Element c_i_1 = i == 0 ? this.h : this.cV.getAt(i - 1);
			//	pV[i + 2] = g.selfApply(rV.getAt(i)).apply(c_i_1.selfApply(ePrimeV.getAt(i)));
			//}
			//MPBridge.b();

			/*MPBridge.a();
			MPBridge.startRecord();
			// - g^r_i * c_i-1^e'_i                [2n]
			Element[] temp = new Element[this.size];
			for (int i = 0; i < this.size; i++) {
				Element c_i_1 = i == 0 ? this.h : this.cV.getAt(i - 1);
				temp[i] = c_i_1.selfApply(ePrimeV.getAt(i));
			}
			mpservice.ModPow[] requests = MPBridge.stopRecord();
			MPBridge.b();
			if(requests.length > 0) {
				java.math.BigInteger[] answers = mpservice.MPService.compute(requests);
				MPBridge.startReplay(answers);
				for (int i = 0; i < this.size; i++) {
					Element c_i_1 = i == 0 ? this.h : this.cV.getAt(i - 1);
					temp[i] = c_i_1.selfApply(ePrimeV.getAt(i));
				}	
				MPBridge.stopReplay();
			}
			MPBridge.reset();*/

			final Element[] temp = new Element[this.size];
			MPBridge.ex(() -> {
				for (int i = 0; i < this.size; i++) {
					Element c_i_1 = i == 0 ? this.h : this.cV.getAt(i - 1);
					temp[i] = c_i_1.selfApply(ePrimeV.getAt(i));
				}
				return temp;
			}, "2");
			

			/*MPBridge.a();
			MPBridge.startRecord();
			for (int i = 0; i < this.size; i++) {
				pV[i + 2] = g.selfApply(rV.getAt(i)).apply(temp[i]);
			}
			requests = MPBridge.stopRecord();
			MPBridge.b();
			if(requests.length > 0) {
				java.math.BigInteger[] answers = mpservice.MPService.compute(requests);
				MPBridge.startReplay(answers);
				for (int i = 0; i < this.size; i++) {
					pV[i + 2] = g.selfApply(rV.getAt(i)).apply(temp[i]);
				}	
				MPBridge.stopReplay();
			}
			MPBridge.reset();
			*/

			MPBridge.ex(() -> {
				for (int i = 0; i < this.size; i++) {
					pV[i + 2] = g.selfApply(rV.getAt(i)).apply(temp[i]);
				}
				return pV;
			}, "2");


			// - Com(0, d)                          [1]
			pV[this.size + 2] = this.gpcs.getRandomizationGenerator().selfApply(d);

			//                                 ---------
			//                                   [3n+3]
			return Tuple.getInstance(pV);
		}

	}

	//===================================================================================
	// getInstance...
	//
	public static PermutationCommitmentProofSystem getInstance(CyclicGroup cyclicGroup, int size) {
		return getInstance(createNonInteractiveSigmaChallengeGenerator(cyclicGroup.getZModOrder()),
						   createNonInteractiveEValuesGenerator(cyclicGroup.getZModOrder(), size),
						   cyclicGroup, size, DEFAULT_KR, DeterministicRandomByteSequence.getInstance());
	}

	public static PermutationCommitmentProofSystem getInstance(CyclicGroup cyclicGroup, int size, DeterministicRandomByteSequence rrbs) {
		return getInstance(
			   createNonInteractiveSigmaChallengeGenerator(cyclicGroup.getZModOrder()),
			   createNonInteractiveEValuesGenerator(cyclicGroup.getZModOrder(), size),
			   cyclicGroup, size, DEFAULT_KR, rrbs);
	}

	public static PermutationCommitmentProofSystem getInstance(CyclicGroup cyclicGroup, int size,
		   Element proverId, int ke, int kc, int kr, DeterministicRandomByteSequence rrbs) {
		return getInstance(
			   createNonInteractiveSigmaChallengeGenerator(kc, proverId),
			   createNonInteractiveEValuesGenerator(ke, size),
			   cyclicGroup, size, kr, rrbs);
	}

	public static PermutationCommitmentProofSystem getInstance(SigmaChallengeGenerator sigmaChallengeGenerator,
		   ChallengeGenerator eValuesGenerator, CyclicGroup cyclicGroup, int size) {
		return getInstance(sigmaChallengeGenerator, eValuesGenerator, cyclicGroup, size, DEFAULT_KR,
						   DeterministicRandomByteSequence.getInstance());
	}

	public static PermutationCommitmentProofSystem getInstance(SigmaChallengeGenerator sigmaChallengeGenerator,
		   ChallengeGenerator eValuesGenerator, CyclicGroup cyclicGroup, int size, int kr,
		   DeterministicRandomByteSequence randomByteSequence) {

		if (cyclicGroup == null || size < 1 || randomByteSequence == null) {
			throw new IllegalArgumentException();
		}
		
		// the generators are calculated lazily only when tuple.get instance is called|
		// at ch.bfh.unicrypt.helper.array.classes.DenseArray.getInstance(DenseArray.java:122)
        // at ch.bfh.unicrypt.math.algebra.general.classes.Tuple.getInstance(Tuple.java:335)
		// MPBridge.a();
		// Tuple generators = Tuple.getInstance(cyclicGroup.getIndependentGenerators(randomByteSequence).limit(size + 1));
		long now = System.currentTimeMillis();
		// Tuple generators2 = Tuple.getInstance(((AbstractCyclicGroup) cyclicGroup).getIndependentGeneratorsMPS(randomByteSequence, 0, size + 1));
		// System.out.println("S: " + (System.currentTimeMillis() - now)); now = System.currentTimeMillis();
		Tuple generators = Tuple.getInstance(((AbstractCyclicGroup) cyclicGroup).getIndependentGeneratorsP(0, size + 1));
		System.out.println("P: " + (System.currentTimeMillis() - now));
		// MPBridge.b();
		
		return getInstance(sigmaChallengeGenerator, eValuesGenerator, generators, kr);
	}

	public static PermutationCommitmentProofSystem getInstance(Tuple independentGenerators) {
		if (independentGenerators == null || independentGenerators.getArity() < 2
			   || !independentGenerators.getFirst().getSet().isCyclic()) {
			throw new IllegalArgumentException();
		}
		CyclicGroup cyclicGroup = (CyclicGroup) independentGenerators.getFirst().getSet();
		int size = independentGenerators.getArity() - 1;
		return getInstance(
			   createNonInteractiveSigmaChallengeGenerator(cyclicGroup.getZModOrder()),
			   createNonInteractiveEValuesGenerator(cyclicGroup.getZModOrder(), size),
			   independentGenerators,
			   DEFAULT_KR);
	}

	public static PermutationCommitmentProofSystem getInstance(Tuple independentGenerators,
		   Element proverId, int ke, int kc, int kr) {
		if (independentGenerators == null || independentGenerators.getArity() < 2
			   || !independentGenerators.getFirst().getSet().isCyclic()) {
			throw new IllegalArgumentException();
		}
		int size = independentGenerators.getArity() - 1;
		return getInstance(
			   createNonInteractiveSigmaChallengeGenerator(kc, (Element) null),
			   createNonInteractiveEValuesGenerator(ke, size),
			   independentGenerators,
			   kr);
	}

	public static PermutationCommitmentProofSystem getInstance(SigmaChallengeGenerator sigmaChallengeGenerator,
		   ChallengeGenerator eValuesGenerator, Tuple independentGenerators, int kr) {

		if (sigmaChallengeGenerator == null || eValuesGenerator == null || independentGenerators == null
			   || independentGenerators.getArity() < 2
			   || !independentGenerators.getSet().isUniform() || !independentGenerators.getFirst().getSet().isCyclic()
			   || kr < 1) {
			throw new IllegalArgumentException();
		}
		CyclicGroup cyclicGroup = (CyclicGroup) independentGenerators.getFirst().getSet();

		if (sigmaChallengeGenerator.getChallengeSpace().getOrder().compareTo(cyclicGroup.getOrder()) > 0) {
			throw new IllegalArgumentException();
		}

		int size = independentGenerators.getArity() - 1;
		Set cs = eValuesGenerator.getChallengeSpace();

		if (!cs.isProduct()
			   || ((ProductSet) cs).getArity() != size
			   || ((ProductSet) cs).getFirst().getOrder().compareTo(cyclicGroup.getOrder()) > 0
			   || !((ProductSet) cs).isUniform()) {
			throw new IllegalArgumentException();
		}

		return new PermutationCommitmentProofSystem(sigmaChallengeGenerator, eValuesGenerator, cyclicGroup, size, kr,
													independentGenerators);
	}
	//===================================================================================
	// Service functions to create non-interactive Sigma- and Element-ChallengeGenerator
	//

	public static RandomOracleSigmaChallengeGenerator
		   createNonInteractiveSigmaChallengeGenerator(final ZMod challengeSpace) {
		return createNonInteractiveSigmaChallengeGenerator(challengeSpace, (Element) null, RandomOracle.getInstance());
	}

	public static RandomOracleSigmaChallengeGenerator
		   createNonInteractiveSigmaChallengeGenerator(final int kc, final Element proverId) {
		return createNonInteractiveSigmaChallengeGenerator(kc, proverId,
														   RandomOracle.getInstance());
	}

	public static RandomOracleSigmaChallengeGenerator
		   createNonInteractiveSigmaChallengeGenerator(final int kc, final Element proverId, RandomOracle randomOracle) {
		if (kc < 1) {
			throw new IllegalArgumentException();
		}
		//[0,...,2^kc - 1] \subseteq Z
		ZMod cs = ZMod.getInstance(MathUtil.powerOfTwo(kc));
		return RandomOracleSigmaChallengeGenerator.getInstance(cs, proverId, randomOracle);
	}

	public static RandomOracleSigmaChallengeGenerator
		   createNonInteractiveSigmaChallengeGenerator(final ZMod challengeSpace, final Element proverId, RandomOracle randomOracle) {
		if (challengeSpace == null) {
			throw new IllegalArgumentException();
		}
		return RandomOracleSigmaChallengeGenerator.getInstance(challengeSpace, proverId, randomOracle);
	}

	public static RandomOracleChallengeGenerator
		   createNonInteractiveEValuesGenerator(final int ke, final int size) {
		return createNonInteractiveEValuesGenerator(ke, size, RandomOracle.getInstance());
	}

	public static RandomOracleChallengeGenerator
		   createNonInteractiveEValuesGenerator(final int ke, final int size, RandomOracle randomOracle) {
		if (size < 1 || ke < 1) {
			throw new IllegalArgumentException();
		}
		// [0,...,2^ke - 1]^N \subseteq Z^N
		return createNonInteractiveEValuesGenerator(ZMod.getInstance(MathUtil.powerOfTwo(ke)), size, randomOracle);
	}

	public static RandomOracleChallengeGenerator
		   createNonInteractiveEValuesGenerator(final ZMod challengeSpace, final int size) {
		return createNonInteractiveEValuesGenerator(challengeSpace, size, RandomOracle.getInstance());
	}

	public static RandomOracleChallengeGenerator
		   createNonInteractiveEValuesGenerator(final ZMod challengeSpace, final int size,
				  RandomOracle randomOracle) {
		if (challengeSpace == null || size < 1) {
			throw new IllegalArgumentException();
		}

		ProductGroup cs = ProductGroup.getInstance(challengeSpace, size);
		return RandomOracleChallengeGenerator.getInstance(cs, randomOracle);
	}

}
