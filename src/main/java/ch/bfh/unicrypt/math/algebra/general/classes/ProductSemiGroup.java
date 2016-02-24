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
package ch.bfh.unicrypt.math.algebra.general.classes;

import ch.bfh.unicrypt.ErrorCode;
import ch.bfh.unicrypt.UniCryptRuntimeException;
import ch.bfh.unicrypt.helper.array.classes.DenseArray;
import ch.bfh.unicrypt.helper.array.interfaces.ImmutableArray;
import ch.bfh.unicrypt.helper.sequence.Sequence;
import ch.bfh.unicrypt.helper.sequence.functions.Operator;
import ch.bfh.unicrypt.helper.sequence.functions.Predicate;
import ch.bfh.unicrypt.math.algebra.general.interfaces.Element;
import ch.bfh.unicrypt.math.algebra.general.interfaces.SemiGroup;
import ch.bfh.unicrypt.math.algebra.general.interfaces.Set;
import java.math.BigInteger;

import mpservice.MPBridge;

/**
 *
 * @author R. Haenni
 */
public class ProductSemiGroup
	   extends ProductSet
	   implements SemiGroup<DenseArray<Element>> {

	private static final long serialVersionUID = 1L;

	protected ProductSemiGroup(DenseArray<Set> sets) {
		super(sets);
	}

	@Override
	public SemiGroup getAt(int index) {
		return (SemiGroup) super.getAt(index);
	}

	@Override
	public SemiGroup getAt(int... indices) {
		return (SemiGroup) super.getAt(indices);
	}

	@Override
	public SemiGroup getFirst() {
		return (SemiGroup) super.getFirst();
	}

	@Override
	public SemiGroup getLast() {
		return (SemiGroup) super.getLast();
	}

	@Override
	public ProductSemiGroup removeAt(final int index) {
		return (ProductSemiGroup) super.removeAt(index);
	}

	public ProductSemiGroup insertAt(final int index, SemiGroup semiGroup) {
		return (ProductSemiGroup) super.insertAt(index, semiGroup);
	}

	public ProductSemiGroup replaceAt(final int index, SemiGroup semiGroup) {
		return (ProductSemiGroup) super.replaceAt(index, semiGroup);
	}

	public ProductSemiGroup add(SemiGroup semiGroup) {
		return (ProductSemiGroup) super.add(semiGroup);
	}

	public ProductSemiGroup append(ProductSemiGroup productSemiGroup) {
		return (ProductSemiGroup) super.append(productSemiGroup);
	}

	@Override
	public ProductSemiGroup extract(int offset, int length) {
		return (ProductSemiGroup) super.extract(offset, length);
	}

	@Override
	public ProductSemiGroup extractPrefix(int length) {
		return (ProductSemiGroup) super.extractPrefix(length);
	}

	@Override
	public ProductSemiGroup extractSuffix(int length) {
		return (ProductSemiGroup) super.extractSuffix(length);
	}

	@Override
	public ProductSemiGroup extractRange(int fromIndex, int toIndex) {
		return (ProductSemiGroup) super.extractRange(fromIndex, toIndex);
	}

	@Override
	public ProductSemiGroup remove(int offset, int length) {
		return (ProductSemiGroup) super.remove(offset, length);
	}

	@Override
	public ProductSemiGroup removePrefix(int length) {
		return (ProductSemiGroup) super.removePrefix(length);
	}

	@Override
	public ProductSemiGroup removeSuffix(int length) {
		return (ProductSemiGroup) super.removeSuffix(length);
	}

	@Override
	public ProductSemiGroup removeRange(int fromIndex, int toIndex) {
		return (ProductSemiGroup) super.removeRange(fromIndex, toIndex);
	}

	@Override
	public ProductSemiGroup reverse() {
		return (ProductSemiGroup) super.reverse();
	}

	@Override
	public ProductSemiGroup[] split(int... indices) {
		return (ProductSemiGroup[]) super.split(indices);
	}

	@Override
	public final Tuple apply(Element element1, Element element2) {
		if (!this.contains(element1) || !this.contains(element2)) {
			throw new UniCryptRuntimeException(ErrorCode.INVALID_ELEMENT, this, element1, element2);
		}
		return this.abstractApply((Tuple) element1, (Tuple) element2);
	}

	@Override
	public final Tuple apply(final Element... elements) {
		if (elements == null) {
			throw new UniCryptRuntimeException(ErrorCode.NULL_POINTER, this, elements);
		}
		return this.defaultApply(Sequence.getInstance(elements));
	}

	@Override
	public final Tuple apply(final ImmutableArray<Element> elements) {
		if (elements == null) {
			throw new UniCryptRuntimeException(ErrorCode.NULL_POINTER, this, elements);
		}
		return this.defaultApply(Sequence.getInstance(elements));
	}

	@Override
	public final Tuple apply(Sequence<Element> elements) {
		if (elements == null) {
			throw new UniCryptRuntimeException(ErrorCode.NULL_POINTER, this, elements);
		}
		return this.defaultApply(elements);
	}

	@Override
	public final Tuple selfApply(Element element, long amount) {
		return this.selfApply(element, BigInteger.valueOf(amount));
	}

	@Override
	public final Tuple selfApply(Element element, Element<BigInteger> amount) {
		if (amount == null) {
			throw new UniCryptRuntimeException(ErrorCode.NULL_POINTER, this, amount);
		}
		return this.selfApply(element, amount.getValue());
	}

	@Override
	public final Tuple selfApply(Element element, BigInteger amount) {
		if (amount == null) {
			throw new UniCryptRuntimeException(ErrorCode.NULL_POINTER, this, amount);
		}
		if (!this.contains(element)) {
			throw new UniCryptRuntimeException(ErrorCode.INVALID_ELEMENT, this, element);
		}
		return this.defaultSelfApply((Tuple) element, amount);
	}

	@Override
	public final Tuple selfApply(Element element) {
		return this.apply(element, element);
	}

	@Override
	public final Tuple multiSelfApply(final Element[] elements, final BigInteger[] amounts) {
		if (elements == null || amounts == null) {
			throw new UniCryptRuntimeException(ErrorCode.NULL_POINTER, this, elements, amounts);
		}
		if (elements.length != amounts.length) {
			throw new UniCryptRuntimeException(ErrorCode.INVALID_LENGTH, this, elements, amounts);
		}
		return this.defaultMultiSelfApply(elements, amounts);
	}

	protected Tuple abstractApply(Tuple tuple1, Tuple tuple2) {
		final Element[] results = new Element[this.getArity()];
		for (int i : this.getAllIndices()) {
			results[i] = tuple1.getAt(i).apply(tuple2.getAt(i));
		}
		return this.abstractGetElement(DenseArray.getInstance(results));
	}

	protected Tuple defaultApply(final Sequence<Element> elements) {
		final ProductSemiGroup semiGroup = this;
		return (Tuple) elements.filter(Predicate.NOT_NULL).reduce(new Operator<Element>() {

			@Override
			public Element apply(Element element1, Element element2) {
				return semiGroup.apply(element1, element2);
			}

		});
	}

	protected Tuple defaultSelfApply(Tuple tuple, BigInteger amount) {
		final Element[] results = new Element[this.getArity()];
		// this comes from PermutationCommitmentProofSystem:333
		if(this.getArity() > 2) {
			MPBridge.ex(() -> {
				for (int i : this.getAllIndices()) {
					results[i] = tuple.getAt(i).selfApply(amount);
				}
				return results;
			}, "2");
		}
		else {
			for (int i : this.getAllIndices()) {
				results[i] = tuple.getAt(i).selfApply(amount);
			}
		}
		

		return this.abstractGetElement(DenseArray.getInstance(results));
	}

	protected Tuple defaultMultiSelfApply(final Element[] elements, final BigInteger[] amounts) {
		if (elements.length == 0) {
			throw new UniCryptRuntimeException(ErrorCode.INVALID_LENGTH, this, elements, amounts);
		}
		Element[] results = new Element[elements.length];
		for (int i = 0; i < elements.length; i++) {
			results[i] = this.selfApply(elements[i], amounts[i]);
		}
		return this.apply(results);
	}

}