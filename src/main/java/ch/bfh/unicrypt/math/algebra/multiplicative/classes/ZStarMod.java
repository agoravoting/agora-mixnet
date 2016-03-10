// drb modpow intercepts to MPBridge
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
package ch.bfh.unicrypt.math.algebra.multiplicative.classes;

import ch.bfh.unicrypt.ErrorCode;
import ch.bfh.unicrypt.UniCryptRuntimeException;
import ch.bfh.unicrypt.helper.converter.classes.biginteger.BigIntegerToBigInteger;
import ch.bfh.unicrypt.helper.converter.interfaces.Converter;
import ch.bfh.unicrypt.helper.factorization.Factorization;
import ch.bfh.unicrypt.helper.math.MathUtil;
import ch.bfh.unicrypt.helper.random.RandomByteSequence;
import ch.bfh.unicrypt.helper.random.hybrid.HybridRandomByteSequence;
import ch.bfh.unicrypt.helper.sequence.Sequence;
import ch.bfh.unicrypt.helper.sequence.functions.Mapping;
import ch.bfh.unicrypt.helper.sequence.functions.Predicate;
import ch.bfh.unicrypt.math.algebra.general.interfaces.Set;
import ch.bfh.unicrypt.math.algebra.multiplicative.abstracts.AbstractMultiplicativeGroup;
import java.math.BigInteger;

import com.squareup.jnagmp.Gmp;
import mpservice.MPBridge;

/**
 * This class implements the group of integers Z*_n with the operation of multiplication modulo n. Its identity element
 * is 1. Every integer in Z*_n is relatively prime to n. The smallest such group is Z*_2 = {1}.
 * <p>
 * @see "Handbook of Applied Cryptography, Definition 2.124"
 * @see <a href="http://en.wikipedia.org/wiki/Multiplicative_group_of_integers_modulo_n">Multiplicative group of
 * integers modulo n</a>
 * <p>
 * @author R. Haenni
 * @author R. E. Koenig
 * @version 2.0
 */
public class ZStarMod
	   extends AbstractMultiplicativeGroup<ZStarModElement, BigInteger> {

	private static final long serialVersionUID = 1L;

	private final BigInteger modulus;
	private final Factorization modulusFactorization;
    
	protected ZStarMod(final BigInteger modulus) {
		this(modulus, Factorization.getInstance());
	}

	protected ZStarMod(final Factorization modulusFactorization) {
		this(modulusFactorization.getValue(), modulusFactorization);
	}

	protected ZStarMod(final BigInteger modulus, final Factorization modulusFactorization) {
		super(BigInteger.class);
		this.modulus = modulus;
		this.modulusFactorization = modulusFactorization;
	}

	/**
	 * Returns the modulus if this group.
	 * <p>
	 * @return The modulus
	 */
	public final BigInteger getModulus() {
		return this.modulus;
	}

	/**
	 * Returns a (possibly incomplete) prime factorization the modulus if this group. An incomplete factorization
	 * implies that the group order is unknown in such a case.
	 * <p>
	 * @return The prime factorization
	 */
	public final Factorization getModulusFactorization() {
		return this.modulusFactorization;
	}

	public final boolean contains(long value) {
		return this.contains(BigInteger.valueOf(value));
	}

	public final ZStarModElement getElement(long value) {
		return this.getElement(BigInteger.valueOf(value));
	}

	@Override
	protected ZStarModElement defaultSelfApplyAlgorithm(final ZStarModElement element, final BigInteger posExponent) {
		// return this.abstractGetElement(element.getValue().modPow(posExponent, this.modulus));
		return this.abstractGetElement(MPBridge.modPow(element.getValue(), posExponent, this.modulus));
	}

	@Override
	protected BigInteger defaultGetOrderUpperBound() {
		return this.getModulus().subtract(MathUtil.ONE);
	}

	@Override
	protected String defaultToStringContent() {
		return this.getModulus().toString();
	}

	@Override
	protected boolean abstractContains(final BigInteger value) {
		return value.signum() > 0
			   && value.compareTo(this.modulus) < 0
			   && MathUtil.areRelativelyPrime(value, this.modulus);
	}

	@Override
	protected ZStarModElement abstractGetElement(BigInteger value) {
		return new ZStarModElement(this, value);
	}

	@Override
	protected Converter<BigInteger, BigInteger> abstractGetBigIntegerConverter() {
		// for reasons of convenience, we start counting at 0
		return BigIntegerToBigInteger.getInstance(0);
	}

	@Override
	protected Sequence<ZStarModElement> abstractGetRandomElements(final RandomByteSequence randomByteSequence) {
		return randomByteSequence
			   .getRandomBigIntegerSequence(MathUtil.ONE, this.getModulus().subtract(MathUtil.ONE))
			   .filter(new Predicate<BigInteger>() {

				   @Override
				   public boolean test(BigInteger value) {
					   return abstractContains(value);
				   }

			   })
			   .map(new Mapping<BigInteger, ZStarModElement>() {

				   @Override
				   public ZStarModElement apply(BigInteger value) {
					   return abstractGetElement(value);
				   }

			   });
	}

	@Override
	protected BigInteger abstractGetOrder() {
		if (!this.getModulusFactorization().getValue().equals(this.getModulus())) {
			return Set.UNKNOWN;
		}
		return MathUtil.eulerFunction(this.getModulus(), this.getModulusFactorization().getPrimeFactors());
	}

	@Override
	protected ZStarModElement abstractGetIdentityElement() {
		return this.abstractGetElement(MathUtil.ONE);
	}

	@Override
	protected ZStarModElement abstractApply(final ZStarModElement element1, final ZStarModElement element2) {
		return this.abstractGetElement(element1.getValue().multiply(element2.getValue()).mod(this.modulus));
	}

	@Override
	public ZStarModElement abstractInvert(final ZStarModElement element) {
		return this.abstractGetElement(element.getValue().modInverse(this.modulus));
	}

	@Override
	protected boolean abstractEquals(final Set set) {
		final ZStarMod zStarMod = (ZStarMod) set;
		return this.getModulus().equals(zStarMod.getModulus());
	}

	@Override
	protected int abstractHashCode() {
		int hash = 7;
		hash = 47 * hash + this.getModulus().hashCode();
		return hash;
	}

	public static ZStarMod getInstance(final long modulus) {
		return ZStarMod.getInstance(BigInteger.valueOf(modulus));
	}

	/**
	 * This is a static factory method to construct a new instance of this class for a given {@code modulus >= 2}. If
	 * {@code modulus} is not prime, then a group of unknown order is returned.
	 * <p>
	 * @param modulus The modulus
	 * @return
	 */
	public static ZStarMod getInstance(final BigInteger modulus) {
		if (modulus == null) {
			throw new UniCryptRuntimeException(ErrorCode.NULL_POINTER, modulus);
		}
		if (modulus.compareTo(MathUtil.ONE) <= 0) {
			throw new UniCryptRuntimeException(ErrorCode.INVALID_ARGUMENT, modulus);
		}
		if (MathUtil.isPrime(modulus)) {
			return ZStarModPrime.getInstance(modulus);
		}
		return new ZStarMod(modulus);
	}

	/**
	 * This is a static factory method to construct a new instance of this class, where the group's modulus is value of
	 * the given prime factorization. This always leads to a group of known order.
	 * <p>
	 * @param modulusFactorization The given prime factorization
	 * @return
	 */
	public static ZStarMod getInstance(final Factorization modulusFactorization) {
		if (modulusFactorization == null) {
			throw new UniCryptRuntimeException(ErrorCode.NULL_POINTER, modulusFactorization);
		}
		if (modulusFactorization.getValue().compareTo(MathUtil.ONE) <= 0) {
			throw new UniCryptRuntimeException(ErrorCode.INVALID_ARGUMENT, modulusFactorization);
		}
		return new ZStarMod(modulusFactorization);
	}

	public static ZStarMod getRandomInstance(int bitLength) {
		return ZStarMod.getRandomInstance(bitLength, HybridRandomByteSequence.getInstance());
	}

	public static ZStarMod getRandomInstance(int bitLength, RandomByteSequence randomByteSequence) {
		if (randomByteSequence == null) {
			throw new UniCryptRuntimeException(ErrorCode.NULL_POINTER, randomByteSequence);
		}
		if (bitLength < 1) {
			throw new UniCryptRuntimeException(ErrorCode.INVALID_BITLENGTH, bitLength);
		}
		return ZStarMod.getInstance(randomByteSequence.getRandomBigIntegerSequence(bitLength).get());
	}

	// drb
    /*public static BigInteger modPow(BigInteger base, BigInteger pow, BigInteger mod) {
        if(MPBridge.debug) new Exception().printStackTrace();
        if(MPBridge.isRecording()) {
            MPBridge.total++;
            MPBridge.addModPow(base, pow, mod);
            return MPBridge.dummy;
        }
        else if(MPBridge.isReplaying()) {
            return MPBridge.getModPow();
        }
        else {
            MPBridge.total++;
            if(gmpModPow) {
                return Gmp.modPowInsecure(base, pow, mod);
            }
            else {
                return base.modPow(pow, mod);    
            }
        }
    }*/
}
