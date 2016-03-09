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
package ch.bfh.unicrypt.math.algebra.general.abstracts;

import ch.bfh.unicrypt.ErrorCode;
import ch.bfh.unicrypt.UniCryptRuntimeException;
import ch.bfh.unicrypt.helper.random.RandomByteSequence;
import ch.bfh.unicrypt.helper.random.deterministic.DeterministicRandomByteSequence;
import ch.bfh.unicrypt.helper.random.hybrid.HybridRandomByteSequence;
import ch.bfh.unicrypt.helper.sequence.Sequence;
import ch.bfh.unicrypt.helper.sequence.functions.Mapping;
import ch.bfh.unicrypt.helper.sequence.functions.Predicate;
import ch.bfh.unicrypt.math.algebra.general.interfaces.CyclicGroup;
import ch.bfh.unicrypt.math.algebra.general.interfaces.Element;
import ch.bfh.unicrypt.helper.array.classes.DenseArray;

import mpservice.MPBridge;

/**
 * This abstract class provides a base implementation for the interface {@link CyclicGroup}.
 * <p>
 * @param <E> The generic type of elements of this cyclic group
 * @param <V> The generic type of values stored in the elements of this cyclic group
 * @see AbstractElement
 * <p>
 * @author R. Haenni
 * @author R. E. Koenig
 * @version 2.0
 */
public abstract class AbstractCyclicGroup<E extends Element<V>, V>
	   extends AbstractGroup<E, V>
	   implements CyclicGroup<V> {

	private static final long serialVersionUID = 1L;

	private E defaultGenerator;

	protected AbstractCyclicGroup(Class<?> valueClass) {
		super(valueClass);
	}

	@Override
	public final E getDefaultGenerator() {
		if (this.defaultGenerator == null) {
			this.defaultGenerator = this.abstractGetDefaultGenerator();
		}
		return this.defaultGenerator;
	}

	@Override
	public final Sequence<E> getIndependentGenerators() {
		return this.getIndependentGenerators(DeterministicRandomByteSequence.getInstance());
	}

	@Override
	public final Sequence<E> getIndependentGenerators(DeterministicRandomByteSequence randomByteSequence) {
		if (randomByteSequence == null) {
			throw new UniCryptRuntimeException(ErrorCode.NULL_POINTER, this, randomByteSequence);
		}
		return this.defaultGetRandomGenerators(randomByteSequence);
	}

	public final DenseArray<Element<V>> getIndependentGeneratorsP(DeterministicRandomByteSequence randomByteSequence, int skip, int size) {
		System.out.println("AbstractCyclicGroup: getIndependentGeneratorsP");

		if (randomByteSequence == null) {
			throw new UniCryptRuntimeException(ErrorCode.NULL_POINTER, this, randomByteSequence);
		}
		long now = System.currentTimeMillis();
		Sequence<E> sequence = this.abstractGetRandomElements(randomByteSequence).skip(skip).limit(size);
		System.out.println(System.currentTimeMillis() - now); now = System.currentTimeMillis();
		java.util.List<E> list = mpservice.MPBridgeS.getIndependentGenerators(sequence);
		System.out.println("get " + (System.currentTimeMillis() - now)); now = System.currentTimeMillis();
		Element<V>[] array = list.toArray(new Element[0]);
		System.out.println("toArray " + (System.currentTimeMillis() - now)); now = System.currentTimeMillis();
 
		return DenseArray.getInstance(array);
	}

	public final DenseArray<Element<V>> getIndependentGeneratorsParallel(DeterministicRandomByteSequence randomByteSequence, int skip, int size) {
		System.out.println("AbstractCyclicGroup: getIndependentGeneratorsParallel");

		if (randomByteSequence == null) {
			throw new UniCryptRuntimeException(ErrorCode.NULL_POINTER, this, randomByteSequence);
		}
		Sequence<E> sequence = this.abstractGetRandomElements(randomByteSequence).skip(skip).limit(size);
		final Element<V>[] array = new Element[size];

		/* not worth it
		MPBridge.ex(() -> {
			int i = 0;
			for (E value : sequence) {
				array[i++] = value;
			}
			return array;
		}, "2");*/

		MPBridge.a();
		int i = 0;
		for (E value : sequence) {
			array[i++] = value;
		}
		MPBridge.b();

		// not worth it
		/*final Element<V>[] array2 = new Element[size];
		MPBridge.ex(() -> {
			for(int j = 0; j < array.length; j++) {
				if(array[j].isGenerator()) {
					array2[j] = array[j];
				}	
				// FIXME
				else {
					throw new RuntimeException();
				}
			}
			return 0;	
		}, "2");*/
		MPBridge.a();
		Element<V>[] array2 = new Element[size];
		for(i = 0; i < array.length; i++) {
			if(array[i].isGenerator()) {
				array2[i] = array[i];
			}	
			// FIXME
			else {
				throw new RuntimeException();
			}
		}
		MPBridge.b();

		return DenseArray.getInstance(array2);
	}

	@Override
	public final E getRandomGenerator() {
		return this.getRandomGenerators().get();
	}

	@Override
	public final E getRandomGenerator(RandomByteSequence randomByteSequence) {
		return this.getRandomGenerators(randomByteSequence).get();
	}

	@Override
	public final Sequence<E> getRandomGenerators() {
		return this.getRandomGenerators(HybridRandomByteSequence.getInstance());
	}

	@Override
	public final Sequence<E> getRandomGenerators(RandomByteSequence randomByteSequence) {
		if (randomByteSequence == null) {
			throw new UniCryptRuntimeException(ErrorCode.NULL_POINTER, this, randomByteSequence);
		}
		return this.defaultGetRandomGenerators(randomByteSequence);
	}

	@Override
	public final boolean isGenerator(Element element) {
		if (!this.contains(element)) {
			throw new UniCryptRuntimeException(ErrorCode.INVALID_ELEMENT, this, element);
		}
		return this.abstractIsGenerator((E) element);
	}

	// see Handbook of Applied Cryptography, Algorithm 4.80 and Note 4.81
	protected Sequence<E> defaultGetRandomGenerators(RandomByteSequence randomByteSequence) {
		return this.abstractGetRandomElements(randomByteSequence).filter(new Predicate<E>() {

			@Override
			public boolean test(E value) {
				// MPBridge.a();
				// boolean ret = isGenerator(value);
				// MPBridge.b();
				// return ret;
				return isGenerator(value);
			}

		});
	}

	@Override
	protected Sequence<E> defaultGetElements() {
		final AbstractCyclicGroup<E, V> group = this;
		return Sequence.getInstance(this.getDefaultGenerator(), new Mapping<E, E>() {

			@Override
			public E apply(E element) {
				return group.apply(group.getDefaultGenerator(), element);
			}

		}).limit(new Predicate<E>() {

			@Override
			public boolean test(E element) {
				return group.getIdentityElement().equals(element);
			}

		});
	}

	protected abstract E abstractGetDefaultGenerator();

	protected abstract boolean abstractIsGenerator(E element);

}
