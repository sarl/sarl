/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.lang.core.util;

import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

/** A specific implementation of a {@code Optional} that allows to
 * change its value.
 * This class is thread-safe. For an implementation that is not
 * thread-safe, the class {@link OutParameter} is provided. It
 * has the similar features as this mutable optional but without
 * synchronized code.
 *
 * @param <T> the type of the data that is stored in the optional.
 * @author <a href="https://github.com/stefanotedeschi">Stefano Tedeschi</a>
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 * @see OutParameter
 */
public final class MutableOptional<T> {

	/**
	 * If non-{@code null}, the value; if {@code null}, indicates no value is present.
	 */
	private T value;

	/**
	 * Returns an empty {@code MutableOptional} instance.  No value is present for this
	 * {@code MutableOptional}.
	 *
	 * <p>Though it may be tempting to do so, avoid testing if an object is empty
	 * by comparing with {@code ==} or {@code !=} against instances returned by
	 * {@code Optional.empty()}.  There is no guarantee that it is a singleton.
	 * Instead, use {@link #isEmpty()} or {@link #isPresent()}.
	 *
	 * @param <T> The type of the non-existent value
	 * @return an empty {@code MutableOptional}
	 */
	public static<T> MutableOptional<T> empty() {
		return new MutableOptional<>(null);
	}

	/**
	 * Returns an empty {@code MutableOptional} instance.  No value is present for this
	 * {@code MutableOptional}.
	 *
	 * <p>Though it may be tempting to do so, avoid testing if an object is empty
	 * by comparing with {@code ==} or {@code !=} against instances returned by
	 * {@code Optional.empty()}.  There is no guarantee that it is a singleton.
	 * Instead, use {@link #isEmpty()} or {@link #isPresent()}.
	 *
	 * @param <T> The type of the non-existent value
	 * @param value the value to put in the {@code MutableOptional}.
	 * @return an empty {@code MutableOptional}
	 */
	public static<T> MutableOptional<T> of(T value) {
		return new MutableOptional<>(value);
	}

	/**
	 * Constructs an instance with the described value.
	 *
	 * @param value the value to describe; it's the caller's responsibility to
	 *        ensure the value is non-{@code null} unless creating the singleton
	 *        instance returned by {@code empty()}.
	 */
	private MutableOptional(T value) {
		this.value = value;
	}

	/**
	 * If a value is present, returns the value, otherwise throws
	 * {@code NoSuchElementException}.
	 *
	 * <p>The preferred alternative to this method is {@link #orElseThrow()}.
	 *
	 * @return the non-{@code null} value described by this {@code Optional}
	 * @throws NoSuchElementException if no value is present
	 */
	public synchronized T get() {
		if (this.value == null) {
			throw new NoSuchElementException("No value present"); //$NON-NLS-1$
		}
		return this.value;
	}

	/**
	 * If a value is present, returns {@code true}, otherwise {@code false}.
	 *
	 * @return {@code true} if a value is present, otherwise {@code false}
	 */
	public synchronized boolean isPresent() {
		return this.value != null;
	}

	/**
	 * If a value is  not present, returns {@code true}, otherwise
	 * {@code false}.
	 *
	 * @return  {@code true} if a value is not present, otherwise {@code false}
	 */
	public synchronized boolean isEmpty() {
		return this.value == null;
	}

	/**
	 * If a value is present, performs the given action with the value,
	 * otherwise does nothing.
	 *
	 * @param action the action to be performed, if a value is present
	 * @throws NullPointerException if value is present and the given action is
	 *         {@code null}
	 */
	public synchronized void ifPresent(Consumer<? super T> action) {
		if (this.value != null) {
			action.accept(this.value);
		}
	}

	/**
	 * If a value is present, performs the given action with the value,
	 * otherwise performs the given empty-based action.
	 *
	 * @param action the action to be performed, if a value is present
	 * @param emptyAction the empty-based action to be performed, if no value is
	 *        present
	 * @throws NullPointerException if a value is present and the given action
	 *         is {@code null}, or no value is present and the given empty-based
	 *         action is {@code null}.
	 */
	public synchronized void ifPresentOrElse(Consumer<? super T> action, Runnable emptyAction) {
		if (this.value != null) {
			action.accept(this.value);
		} else {
			emptyAction.run();
		}
	}

	/**
	 * If a value is present, and the value matches the given predicate,
	 * returns an {@code Optional} describing the value, otherwise returns an
	 * empty {@code MutableOptional}.
	 *
	 * @param predicate the predicate to apply to a value, if present
	 * @return an {@code Optional} describing the value of this
	 *         {@code Optional}, if a value is present and the value matches the
	 *         given predicate, otherwise an empty {@code Optional}
	 * @throws NullPointerException if the predicate is {@code null}
	 */
	public synchronized MutableOptional<T> filter(Predicate<? super T> predicate) {
		assert predicate != null;
		if (!isPresent()) {
			return this;
		}
		return predicate.test(this.value) ? this : empty();
	}

	/**
	 * If a value is present, returns an {@code MutableOptional} describing
	 * the result of applying the given mapping function to
	 * the value, otherwise returns an empty {@code Optional}.
	 *
	 * <p>If the mapping function returns a {@code null} result then this method
	 * returns an empty {@code Optional}.
	 *
	 * <p>This method supports post-processing on {@code Optional} values, without
	 * the need to explicitly check for a return status.  For example, the
	 * following code traverses a stream of URIs, selects one that has not
	 * yet been processed, and creates a path from that URI, returning
	 * an {@code MutableOptional<Path>}:
	 *
	 * <pre>{@code
	 *     MutableOptional<Path> p =
	 *         uris.stream().filter(uri -> !isProcessedYet(uri))
	 *                       .findFirst()
	 *                       .map(Paths::get);
	 * }</pre>
	 *
	 * Here, {@code findFirst} returns an {@code MutableOptional<URI>}, and then
	 * {@code map} returns an {@code MutableOptional<Path>} for the desired
	 * URI if one exists.
	 *
	 * @param mapper the mapping function to apply to a value, if present
	 * @param <U> The type of the value returned from the mapping function
	 * @return an {@code MutableOptional} describing the result of applying a mapping
	 *         function to the value of this {@code MutableOptional}, if a value is
	 *         present, otherwise an empty {@code MutableOptional}
	 * @throws NullPointerException if the mapping function is {@code null}
	 */
	public synchronized <U> MutableOptional<U> map(Function<? super T, ? extends U> mapper) {
		assert mapper != null;
		if (!isPresent()) {
			return empty();
		}
		return new MutableOptional<>(mapper.apply(this.value));
	}

	/**
	 * If a value is present, returns the result of applying the given
	 * {@code Optional}-bearing mapping function to the value, otherwise returns
	 * an empty {@code Optional}.
	 *
	 * <p>This method is similar to {@link #map(Function)}, but the mapping
	 * function is one whose result is already an {@code Optional}, and if
	 * invoked, {@code flatMap} does not wrap it within an additional
	 * {@code Optional}.
	 *
	 * @param <U> The type of value of the {@code Optional} returned by the
	 *            mapping function
	 * @param mapper the mapping function to apply to a value, if present
	 * @return the result of applying an {@code Optional}-bearing mapping
	 *         function to the value of this {@code Optional}, if a value is
	 *         present, otherwise an empty {@code Optional}
	 * @throws NullPointerException if the mapping function is {@code null} or
	 *         returns a {@code null} result
	 */
	public synchronized <U> MutableOptional<U> flatMap(Function<? super T, ? extends MutableOptional<? extends U>> mapper) {
		assert mapper != null;
		if (!isPresent()) {
			return empty();
		}
		@SuppressWarnings("unchecked")
		final var r = (MutableOptional<U>) mapper.apply(this.value);
		assert r != null;
		return r;
	}

	/**
	 * If a value is present, returns an {@code Optional} describing the value,
	 * otherwise returns an {@code Optional} produced by the supplying function.
	 *
	 * @param supplier the supplying function that produces an {@code Optional}
	 *        to be returned
	 * @return returns an {@code Optional} describing the value of this
	 *         {@code Optional}, if a value is present, otherwise an
	 *         {@code Optional} produced by the supplying function.
	 * @throws NullPointerException if the supplying function is {@code null} or
	 *         produces a {@code null} result.
	 */
	public synchronized MutableOptional<T> or(Supplier<? extends MutableOptional<? extends T>> supplier) {
		assert supplier != null;
		if (isPresent()) {
			return this;
		}
		@SuppressWarnings("unchecked")
		final var r = (MutableOptional<T>) supplier.get();
		assert r != null;
		return r;
	}

	/**
	 * If a value is present, returns the value, otherwise returns
	 * {@code other}.
	 *
	 * @param other the value to be returned, if no value is present.
	 *        May be {@code null}.
	 * @return the value, if present, otherwise {@code other}
	 */
	public synchronized T orElse(T other) {
		return this.value != null ? this.value : other;
	}

	/**
	 * If a value is present, returns the value, otherwise returns the result
	 * produced by the supplying function.
	 *
	 * @param supplier the supplying function that produces a value to be returned
	 * @return the value, if present, otherwise the result produced by the
	 *         supplying function
	 * @throws NullPointerException if no value is present and the supplying
	 *         function is {@code null}
	 */
	public synchronized T orElseGet(Supplier<? extends T> supplier) {
		return this.value != null ? this.value : supplier.get();
	}

	/**
	 * If a value is present, returns the value, otherwise throws
	 * {@code NoSuchElementException}.
	 *
	 * @return the non-{@code null} value described by this {@code Optional}
	 * @throws NoSuchElementException if no value is present.
	 */
	public synchronized T orElseThrow() {
		if (this.value == null) {
			throw new NoSuchElementException("No value present"); //$NON-NLS-1$
		}
		return this.value;
	}

	/**
	 * If a value is present, returns the value, otherwise throws an exception
	 * produced by the exception supplying function.
	 *
	 * A method reference to the exception constructor with an empty argument
	 * list can be used as the supplier. For example,
	 * {@code IllegalStateException::new}
	 *
	 * @param <X> Type of the exception to be thrown
	 * @param exceptionSupplier the supplying function that produces an
	 *        exception to be thrown
	 * @return the value, if present
	 * @throws X if no value is present
	 * @throws NullPointerException if no value is present and the exception
	 *          supplying function is {@code null}
	 */
	public synchronized <X extends Throwable> T orElseThrow(Supplier<? extends X> exceptionSupplier) throws X {
		assert exceptionSupplier != null;
		if (this.value != null) {
			return this.value;
		}
		throw exceptionSupplier.get();
	}

	/**
	 * Indicates whether some other object is "equal to" this {@code Optional}.
	 * The other object is considered equal if:
	 * <ul>
	 * <li>it is also an {@code Optional} and;
	 * <li>both instances have no value present or;
	 * <li>the present values are "equal to" each other via {@code equals()}.
	 * </ul>
	 *
	 * @param obj an object to be tested for equality
	 * @return {@code true} if the other object is "equal to" this object
	 *         otherwise {@code false}
	 */
	@Override
	public synchronized boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		return obj instanceof MutableOptional<?> other
				&& Objects.equals(this.value, other.value);
	}

	/**
	 * Returns the hash code of the value, if present, otherwise {@code 0}
	 * (zero) if no value is present.
	 *
	 * @return hash code value of the present value or {@code 0} if no value is
	 *         present
	 */
	@Override
	public synchronized int hashCode() {
		return Objects.hashCode(this.value);
	}

	/**
	 * Returns a non-empty string representation of this {@code Optional}
	 * suitable for debugging.  The exact presentation format is unspecified and
	 * may vary between implementations and versions.
	 *
	 * If a value is present the result must include its string representation
	 * in the result.  Empty and present {@code Optional}s must be unambiguously
	 * differentiable.
	 *
	 * @return the string representation of this instance
	 */
	@Override
	public String toString() {
		return this.value != null
				? ("Optional[" + this.value + "]") //$NON-NLS-1$ //$NON-NLS-2$
						: "Optional.empty"; //$NON-NLS-1$
	}

	/** Change the value stored in this {@code MutableOptional}.
	 *
	 * @param value the new value. If it is {@code null}, then the {@code MutableOptional} becomes empty.
	 */
	public synchronized void set(T value) {
		this.value = value;
	}

}
