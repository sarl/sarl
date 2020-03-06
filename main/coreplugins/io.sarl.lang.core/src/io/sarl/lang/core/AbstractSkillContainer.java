/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2020 the original authors or authors.
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

package io.sarl.lang.core;

import java.lang.reflect.Constructor;
import java.security.InvalidParameterException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import com.google.common.reflect.TypeToken;
import org.eclipse.xtext.xbase.lib.Functions.Function2;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.util.OutParameter;

/** The definition of the notion of skill container in SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public abstract class AbstractSkillContainer extends AgentProtectedAPIObject implements Identifiable {

	private static final DynamicSkillProvider SINGLETON = (it0, it1) -> null;

	/** Skill repository.
	 */
	private final ConcurrentMap<Class<? extends Capacity>, AtomicSkillReference> skillRepository = new ConcurrentHashMap<>();

	private DynamicSkillProvider skillProvider;

	/**
	 * Creates a new agent with a parent <code>parentID</code> without initializing the built-in capacities.
	 *
	 * @param skillProvider provides the skills dynamically on demand.
	 * @since 0.6
	 */
	public AbstractSkillContainer(
			DynamicSkillProvider skillProvider) {
		if (skillProvider == null) {
			this.skillProvider = SINGLETON;
		} else {
			this.skillProvider = skillProvider;
		}
	}

	/** Change te dynamic skill provider.
	 *
	 * <p>This function is part of the private API of the library.
	 *
	 * @param provider the skill provider.
	 * @since 0.6
	 */
	void $setDynamicSkillProvider(DynamicSkillProvider provider) {
		if (provider == null) {
			this.skillProvider = SINGLETON;
		} else {
			this.skillProvider = provider;
		}
	}

	/** Replies the skill repository.
	 *
	 * <p>This function is part of the private API of the library.
	 *
	 * @return the skill repository.
	 */
	final ConcurrentMap<Class<? extends Capacity>, AtomicSkillReference> $getSkillRepository() {
		return this.skillRepository;
	}

	@Override
	@SafeVarargs
	protected final <S extends Skill> S setSkill(S skill, Class<? extends Capacity>... capacities) {
		$setSkill(skill, capacities);
		return skill;
	}

	/** Add a skill to the agent.
	 *
	 * @param skill the new skill.
	 * @param capacities the implemented capacities by the skill.
	 * @return the reference to the skill.
	 * @since 16.0
	 */
	@SafeVarargs
	protected final AtomicSkillReference $setSkill(Skill skill, Class<? extends Capacity>... capacities) {
		assert skill != null : "the skill parameter must not be null"; //$NON-NLS-1$
		$attachOwner(skill);
		AtomicSkillReference newRef = null;
		if (capacities == null || capacities.length == 0) {
			// No capacity was provided as argument, the implemented capacities are automatically extracted
			newRef = runOnImplementedCapacities(skill, newRef, (capacity, nr) -> registerSkill(skill, capacity, nr));
		} else {
			for (final Class<? extends Capacity> capacity : capacities) {
				assert capacity != null : "the capacity parameter must not be null"; //$NON-NLS-1$
				assert capacity.isInterface() : "the capacity parameter must be an interface"; //$NON-NLS-1$
				if (!capacity.isInstance(skill)) {
					throw new InvalidParameterException(
							"the skill must implement the given capacity " //$NON-NLS-1$
							+ capacity.getName());
				}
				newRef = registerSkill(skill, capacity, newRef);
			}
		}
		return newRef;
	}

	/** Attach the owner of the skill to the given skill.
	 *
	 * @param skill the skill to attach to its owner.
	 * @since 0.11
	 */
	protected abstract void $attachOwner(Skill skill);

	private AtomicSkillReference registerSkill(Skill skill, Class<? extends Capacity> capacity, AtomicSkillReference firstRef) {
		final AtomicSkillReference newReference = new AtomicSkillReference(skill);
		final AtomicSkillReference oldReference = this.$getSkillRepository().put(capacity, newReference);
		if (oldReference != null) {
			oldReference.clear();
		}
		if (firstRef == null) {
			return newReference;
		}
		return firstRef;
	}

	private static AtomicSkillReference runOnImplementedCapacities(Skill skill, AtomicSkillReference oldRef,
			Function2<Class<? extends Capacity>, AtomicSkillReference, AtomicSkillReference> callback) {
		final OutParameter<AtomicSkillReference> ref = new OutParameter<>(oldRef);
		TypeToken.of(skill.getClass()).getTypes().interfaces().stream().forEach(it -> {
			final Class<?> type = it.getRawType();
			if (Capacity.class.isAssignableFrom(type) && !Capacity.class.equals(type)) {
				ref.set(callback.apply(type.asSubclass(Capacity.class), ref.get()));
			}
		});
		return ref.get();
	}

	@Override
	@Pure
	protected final <S extends Capacity> S getSkill(Class<S> capacity) {
		assert capacity != null;
		final AtomicSkillReference skill = $getSkill(capacity);
		assert skill != null;
		return $castSkill(capacity, skill);
	}

	/** Cast the skill reference to the given capacity type.
	 *
	 * @param <S> the expected capacity type.
	 * @param capacity the expected capacity type.
	 * @param skillReference the skill reference.
	 * @return the skill casted to the given capacity.
	 */
	@Pure
	protected <S extends Capacity> S $castSkill(Class<S> capacity, AtomicSkillReference skillReference) {
		final S skill = capacity.cast(skillReference.get());
		if (skill == null) {
			throw new UnimplementedCapacityException(capacity, getID());
		}
		return skill;
	}

	@Override
	@Pure
	protected AtomicSkillReference $getSkill(Class<? extends Capacity> capacity) {
		AtomicSkillReference skill = $getSkillRepository().get(capacity);
		// Check if the stored skill is still not empty
		if (skill != null) {
			final Skill s = skill.get();
			if (s != null) {
				return skill;
			}
		}
		// Try to load dynamically the skill
		skill = createSkillFromDynamicProvider(capacity);
		if (skill != null) {
			return skill;
		}
		// Use the default skill declaration if present.
		skill = createSkillFromDefaultSkillAnnotation(capacity);
		if (skill != null) {
			return skill;
		}
		throw new UnimplementedCapacityException(capacity, getID());
	}

	private AtomicSkillReference createSkillFromDynamicProvider(Class<? extends Capacity> capacity) {
		assert this.skillProvider != null;
		final AtomicSkillReference reference = this.skillProvider.installSkill(this, capacity);
		if (reference != null) {
			final Skill s = reference.get();
			if (s != null) {
				return reference;
			}
		}
		return null;
	}

	private AtomicSkillReference createSkillFromDefaultSkillAnnotation(Class<? extends Capacity> capacity) {
		final DefaultSkill annotation = capacity.getAnnotation(DefaultSkill.class);
		if (annotation != null) {
			try {
				final Class<? extends Skill> type = annotation.value();
				Constructor<? extends Skill> cons;
				try {
					cons = type.getConstructor(AbstractSkillContainer.class);
					cons.setAccessible(true);
					final Skill skillInstance = cons.newInstance(this);
					return $setSkill(skillInstance);
				} catch (Throwable exception) {
					cons = type.getConstructor();
				}
				cons.setAccessible(true);
				final Skill skillInstance = cons.newInstance();
				// Change the skill recursively
				final AtomicSkillReference ref = $setSkill(skillInstance);
				if (ref != null) {
					final Skill s = ref.get();
					if (s != null) {
						return ref;
					}
				}
			} catch (Throwable exception) {
				throw new UnimplementedCapacityException(capacity, getID(), exception);
			}
		}
		return null;
	}

	@Override
	@Pure
	protected boolean hasSkill(Class<? extends Capacity> capacity) {
		assert capacity != null;
		if (!$getSkillRepository().containsKey(capacity)) {
			if (this.skillProvider != null) {
				final AtomicSkillReference reference = this.skillProvider.installSkill(this, capacity);
				if (reference != null && reference.get() != null) {
					return true;
				}
			}
			final DefaultSkill annotation = capacity.getAnnotation(DefaultSkill.class);
			return annotation != null && annotation.value() != null;
		}
		return true;
	}

	@Override
	protected <S extends Capacity> S clearSkill(Class<S> capacity) {
		assert capacity != null;
		final AtomicSkillReference reference = $getSkillRepository().remove(capacity);
		if (reference != null) {
			final Skill skill = reference.clear();
			if (skill != null) {
				return capacity.cast(skill);
			}
		}
		return null;
	}

	@Override
	@Inline("setSkill($2, $1)")
	protected <S extends Skill> void operator_mappedTo(Class<? extends Capacity> capacity, S skill) {
		setSkill(skill, capacity);
	}

}
