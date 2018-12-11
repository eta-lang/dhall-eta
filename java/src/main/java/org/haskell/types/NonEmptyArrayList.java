/**
 * 
 */
package org.haskell.types;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Spliterator;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;

/**
 * @author Javier Neira Sanchez
 *
 */
public class NonEmptyArrayList<E> implements List<E>{
	
	@SafeVarargs
	public static <E> NonEmptyArrayList<E> of(E... elems) {
		return new NonEmptyArrayList<E>(elems);
	}
	
	public static <E> NonEmptyArrayList<E> of(List<E> col) {
		return new NonEmptyArrayList<E>(col);
	}
	
	private final ArrayList<E> wrapped;

	public NonEmptyArrayList (E elem) {
		this(Collections.singletonList(elem));
	}
	
	@SafeVarargs
	public NonEmptyArrayList (E... elems) {
		this(Arrays.asList(elems));
	}
	
	public NonEmptyArrayList (List<E> col) {
		super();
		if (col == null || col.isEmpty())
			throw new IllegalArgumentException("NonEmptyList can't be empty!");
		this.wrapped = new ArrayList<E>(col);
	}
	
	public void trimToSize() {
		wrapped.trimToSize();
	}

	public void ensureCapacity(int minCapacity) {
		wrapped.ensureCapacity(minCapacity);
	}

	public boolean containsAll(Collection<?> c) {
		return wrapped.containsAll(c);
	}

	public int size() {
		return wrapped.size();
	}

	public boolean isEmpty() {
		return wrapped.isEmpty();
	}

	public boolean contains(Object o) {
		return wrapped.contains(o);
	}

	public int indexOf(Object o) {
		return wrapped.indexOf(o);
	}

	public int lastIndexOf(Object o) {
		return wrapped.lastIndexOf(o);
	}

	public Object clone() {
		return wrapped.clone();
	}

	public Object[] toArray() {
		return wrapped.toArray();
	}

	public <T> T[] toArray(T[] a) {
		return wrapped.toArray(a);
	}

	public String toString() {
		return "NonEmptyArrayList ["+wrapped+"]";
	}

	public E get(int index) {
		return wrapped.get(index);
	}

	public E set(int index, E element) {
		return wrapped.set(index, element);
	}

	public boolean add(E e) {
		return wrapped.add(e);
	}

	public boolean equals(Object o) {
		return wrapped.equals(o);
	}

	public void add(int index, E element) {
		wrapped.add(index, element);
	}

	public E remove(int index) {
		ArrayList<E> aux = new ArrayList<E>(wrapped);
		aux.remove(index);
		assertIsNotEmpty(aux);
		return wrapped.remove(index);
	}

	public boolean remove(Object o) {
		ArrayList<E> aux = new ArrayList<E>(wrapped);
		aux.remove(o);
		assertIsNotEmpty(aux);
		return wrapped.remove(o);
	}

	public int hashCode() {
		return wrapped.hashCode();
	}

	public void clear() {
		throw new IllegalStateException("NonEmptyList can't be empty!");
	}

	public boolean addAll(Collection<? extends E> c) {
		return wrapped.addAll(c);
	}

	public boolean addAll(int index, Collection<? extends E> c) {
		return wrapped.addAll(index, c);
	}

	public boolean removeAll(Collection<?> c) {
		ArrayList<E> aux = new ArrayList<E>(wrapped);
		boolean result = aux.removeAll(c);
		if (result)
			assertIsNotEmpty(aux);
		return wrapped.removeAll(c);
	}

	private void assertIsNotEmpty(ArrayList<E> aux) {
		if (aux.isEmpty())
			throw new IllegalStateException("NonEmptyList can't be empty!");
	}

	public boolean retainAll(Collection<?> c) {
		return wrapped.retainAll(c);
	}

	public Stream<E> stream(){
		return wrapped.stream();
	}

	public Stream<E> parallelStream(){
		return wrapped.parallelStream();
	}

	public ListIterator<E> listIterator(int index) {
		return wrapped.listIterator(index);
	}

	public ListIterator<E> listIterator() {
		return wrapped.listIterator();
	}

	public Iterator<E> iterator() {
		return wrapped.iterator();
	}

	public List<E> subList(int fromIndex, int toIndex) {
		return wrapped.subList(fromIndex, toIndex);
	}

	public void forEach(Consumer<? super E> action) {
		wrapped.forEach(action);
	}

	public Spliterator<E> spliterator() {
		return wrapped.spliterator();
	}

	public boolean removeIf(Predicate<? super E> filter) {
		ArrayList<E> aux = new ArrayList<E>(wrapped);
		boolean result = aux.removeIf(filter);
		if (result)
			assertIsNotEmpty(aux);
		return wrapped.removeIf(filter);
	}

	public void replaceAll(UnaryOperator<E> operator) {
		wrapped.replaceAll(operator);
	}

	public void sort(Comparator<? super E> c) {
		wrapped.sort(c);
	}
	
	
	
}
