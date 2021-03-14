/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.lang.async;

import java.util.concurrent.BlockingDeque;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentNavigableMap;
import java.util.concurrent.ConcurrentSkipListMap;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicIntegerArray;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicLongArray;
import java.util.concurrent.atomic.AtomicMarkableReference;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.atomic.DoubleAccumulator;
import java.util.concurrent.atomic.DoubleAdder;
import java.util.concurrent.atomic.LongAccumulator;
import java.util.concurrent.atomic.LongAdder;
import javax.inject.Inject;

import com.google.common.collect.ConcurrentHashMultiset;
import com.google.common.util.concurrent.AtomicDouble;
import com.google.common.util.concurrent.AtomicDoubleArray;
import com.google.common.util.concurrent.AtomicLongMap;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

import io.sarl.lang.util.ConcurrentCollection;
import io.sarl.lang.util.ConcurrentList;
import io.sarl.lang.util.ConcurrentSet;
import io.sarl.lang.util.Utils;

/**
 * Standard helper that determine if a field is synchronized or not.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public class StandardSynchronizedFieldDetector implements ISynchronizedFieldDetector {

	private static final Class<?>[] SYNCHRONIZED_TYPES = {
		AtomicBoolean.class,
		AtomicDouble.class,
		AtomicDoubleArray.class,
		AtomicInteger.class,
		AtomicIntegerArray.class,
		AtomicLong.class,
		AtomicLongArray.class,
		AtomicLongMap.class,
		AtomicMarkableReference.class,
		AtomicReference.class,
		BlockingDeque.class,
		BlockingQueue.class,
		ConcurrentCollection.class,
		ConcurrentHashMap.class,
		ConcurrentHashMultiset.class,
		ConcurrentLinkedDeque.class,
		ConcurrentLinkedQueue.class,
		ConcurrentList.class,
		ConcurrentMap.class,
		ConcurrentNavigableMap.class,
		ConcurrentSet.class,
		ConcurrentSkipListMap.class,
		ConcurrentSkipListSet.class,
		CopyOnWriteArrayList.class,
		CopyOnWriteArraySet.class,
		CountDownLatch.class,
		DoubleAccumulator.class,
		DoubleAdder.class,
		LongAccumulator.class,
		LongAdder.class,
	};

	@Inject
	private CommonTypeComputationServices types;

	@Pure
	@Override
	public boolean isSynchronizedField(XtendField field) {
		final LightweightTypeReference fieldType = Utils.toLightweightTypeReference(field.getType(), this.types).getPrimitiveIfWrapperType();
		if (fieldType.isPrimitive()) {
			return field.isFinal() || field.isVolatile();
		}
		for (final Class<?> type : SYNCHRONIZED_TYPES) {
			final JvmType ltype = this.types.getTypeReferences().findDeclaredType(type, field);
			if (ltype != null && fieldType.isSubtypeOf(type)) {
				return true;
			}
		}
		return false;
	}

}
