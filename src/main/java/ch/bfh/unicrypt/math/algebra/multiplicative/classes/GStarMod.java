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
import ch.bfh.unicrypt.helper.factorization.SpecialFactorization;
import ch.bfh.unicrypt.helper.math.MathUtil;
import ch.bfh.unicrypt.helper.random.RandomByteSequence;
import ch.bfh.unicrypt.helper.sequence.Sequence;
import ch.bfh.unicrypt.helper.sequence.functions.Mapping;
import ch.bfh.unicrypt.math.algebra.general.interfaces.Set;
import ch.bfh.unicrypt.math.algebra.multiplicative.abstracts.AbstractMultiplicativeCyclicGroup;
import java.math.BigInteger;

// drb
import com.squareup.jnagmp.Gmp;

/**
 * This interface represents the concept of a sub-group G_m (of order m) of a cyclic group of integers Z*_n with the
 * operation of multiplication modulo n. For Z*_n to be cyclic, n must be 2, 4, p^e, or 2p^e, where p>2 is prime and
 * e>0. The actual sub-group depends on the given set of prime factors of the order phi(n) of Z*_n, where phi(n) is the
 * Euler totient function. The order m=|G_m| is the product of all given prime factors of phi(n). If all prime factors
 * of phi(n) are given, which implies m=phi(n), then G_m is the parent group Z*_n.
 * <p>
 * <p/>
 * @see "Handbook of Applied Cryptography, Fact 2.132"
 * @see "Handbook of Applied Cryptography, Definition 2.100"
 * @see "Handbook of Applied Cryptography, Definition 2.166"
 * <p>
 * @author R. Haenni
 * @author R. E. Koenig
 * @version 2.0
 */
public class GStarMod
       extends AbstractMultiplicativeCyclicGroup<GStarModElement, BigInteger> {

    private static final long serialVersionUID = 1L;

    private final BigInteger modulus;
    private final SpecialFactorization modulusFactorization;
    private final Factorization orderFactorization;
    private ZStarMod superGroup;

    // drb
    public static boolean gmpModPow = false;

    protected GStarMod(SpecialFactorization modulusFactorization, Factorization orderFactorization) {
        super(BigInteger.class);
        this.modulus = modulusFactorization.getValue();
        this.modulusFactorization = modulusFactorization;
        this.orderFactorization = orderFactorization;
    }

    /**
     * Returns the modulus if this group.
     * <p>
     * <p/>
     * @return The modulus
     */
    public final BigInteger getModulus() {
        return this.modulus;
    }

    /**
     * Returns a (possibly incomplete) prime factorization the modulus if this group. An incomplete factorization
     * implies that the group order is unknown in such a case.
     * <p>
     * <p/>
     * @return The prime factorization
     */
    public final SpecialFactorization getModulusFactorization() {
        return this.modulusFactorization;
    }

    /**
     * Returns prime factorization of the group order phi(n) of Z*_n.
     * <p>
     * <p/>
     * @return The prime factorization of the group order
     */
    public final Factorization getOrderFactorization() {
        return this.orderFactorization;
    }

    public final ZStarMod getZStarMod() {
        if (this.superGroup == null) {
            this.superGroup = ZStarMod.getInstance(this.getModulusFactorization());
        }
        return this.superGroup;
    }

    public final boolean contains(long value) {
        return this.contains(BigInteger.valueOf(value));
    }

    public final GStarModElement getElement(long value) {
        return this.getElement(BigInteger.valueOf(value));
    }

    /**
     * Returns the quotient k=phi(n)/m of the orders of the two involved groups.
     * <p>
     * <p/>
     * @return The quotient of the two orders.
     */
    public BigInteger getCoFactor() {
        return this.getZStarMod().getOrder().divide(this.getOrder());
    }

    @Override
    protected GStarModElement defaultSelfApplyAlgorithm(final GStarModElement element, final BigInteger posExponent) {
        // return this.abstractGetElement(element.getValue().modPow(posExponent, this.modulus));
        return this.abstractGetElement(modPow(element.getValue(), posExponent, this.modulus));
    }

    @Override
    protected String defaultToStringContent() {
        return this.getModulus().toString() + "," + this.getOrder().toString();
    }

    @Override
    protected boolean abstractContains(final BigInteger value) {
        return value.signum() > 0
               && value.compareTo(this.modulus) < 0
               && MathUtil.areRelativelyPrime(value, this.modulus)
               && modPow(value, this.getOrder(), this.modulus).equals(MathUtil.ONE);
               // && value.modPow(this.getOrder(), this.modulus).equals(MathUtil.ONE);
    }

    @Override
    protected GStarModElement abstractGetElement(BigInteger value) {
        return new GStarModElement(this, value);
    }

    @Override
    protected Converter<BigInteger, BigInteger> abstractGetBigIntegerConverter() {
        return BigIntegerToBigInteger.getInstance(0);
    }

    @Override
    protected Sequence<GStarModElement> abstractGetRandomElements(final RandomByteSequence randomByteSequence) {
        return this.getZStarMod().abstractGetRandomElements(randomByteSequence).map(new Mapping<ZStarModElement, GStarModElement>() {

            @Override
            public GStarModElement apply(ZStarModElement element) {
                return abstractGetElement(element.power(getCoFactor()).getValue());
            }
        });
    }

    @Override
    protected BigInteger abstractGetOrder() {
        return this.getOrderFactorization().getValue();
    }

    @Override
    protected GStarModElement abstractGetIdentityElement() {
        return this.abstractGetElement(MathUtil.ONE);
    }

    @Override
    protected GStarModElement abstractApply(final GStarModElement element1, final GStarModElement element2) {
        return this.abstractGetElement(element1.getValue().multiply(element2.getValue()).mod(this.modulus));
    }

    @Override
    protected GStarModElement abstractInvert(final GStarModElement element) {
        return this.abstractGetElement(element.getValue().modInverse(this.modulus));
    }

    @Override
    protected GStarModElement abstractGetDefaultGenerator() {
        // see http://en.wikipedia.org/wiki/Schnorr_group
        BigInteger alpha = MathUtil.ZERO;
        GStarModElement element;
        do {
            do {
                alpha = alpha.add(MathUtil.ONE);
            } while (!MathUtil.areRelativelyPrime(alpha, this.getModulus()));
            element = this.abstractGetElement(alpha.modPow(this.getCoFactor(), this.modulus));
        } while (!this.isGenerator(element)); // this test could be skipped for a prime order
        return element;
    }

    // see Handbook of Applied Cryptography, Algorithm 4.80 and Note 4.81
    // the implemented method is a mix between 4.80 and 4.81
    // See also http://en.wikipedia.org/wiki/Schnorr_group
    @Override
    protected boolean abstractIsGenerator(GStarModElement element) {
        for (final BigInteger prime : this.getOrderFactorization().getPrimeFactors()) {
            if (element.selfApply(this.getOrder().divide(prime)).isEquivalent(this.getIdentityElement())) {
                return false;
            }
        }
        return true;
    }

    @Override
    protected boolean abstractEquals(Set set) {
        final GStarMod other = (GStarMod) set;
        return this.getModulus().equals(other.getModulus()) && this.getOrder().equals(other.getOrder());
    }

    @Override
    protected int abstractHashCode() {
        int hash = 7;
        hash = 47 * hash + this.getModulus().hashCode();
        hash = 47 * hash + this.getOrder().hashCode();
        return hash;
    }

    /**
     * This is the general static factory method for this class.
     * <p>
     * <p/>
     * @param modulusFactorization
     * @param orderFactorization
     * @return
     */
    public static GStarMod getInstance(SpecialFactorization modulusFactorization, Factorization orderFactorization) {
        GStarMod group = new GStarMod(modulusFactorization, orderFactorization);
        if (!group.getOrder().mod(orderFactorization.getValue()).equals(MathUtil.ZERO)) {
            throw new UniCryptRuntimeException(ErrorCode.INCOMPATIBLE_ARGUMENTS, modulusFactorization, orderFactorization);
        }
        return group;
    }

    // drb
    public static BigInteger modPow(BigInteger base, BigInteger pow, BigInteger mod) {
        if(ch.MP.debug) new Exception().printStackTrace();
        if(ch.MP.isRecording()) {
            ch.MP.total++;
            ch.MP.addModPow(base, pow, mod);
            return ch.MP.dummy;
        }
        else if(ch.MP.isReplaying()) {
            return ch.MP.getModPow();
        }
        else {
            ch.MP.total++;
            if(gmpModPow) {
                return Gmp.modPowInsecure(base, pow, mod);
            }
            else {
                return base.modPow(pow, mod);    
            }
        }
    }
}