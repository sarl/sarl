/**
 */
package io.sarl.lang.sarl.impl;

import java.util.Collection;

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlPackage;

import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;
import org.eclipse.xtend.core.xtend.impl.XtendFunctionImplCustom;
import org.eclipse.xtext.common.types.JvmTypeReference;

/**
 * <!-- begin-user-doc -->
 * Custom implementation of the SarlAction grammar element.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link #getFiredEvents <em>Fired Events</em>}</li>
 * </ul>
 * </p>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SarlActionImplCustom extends XtendFunctionImplCustom implements SarlAction {

	/**
	 * The cached value of the '{@link #getFiredEvents() <em>Fired Events</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getFiredEvents()
	 * @ordered
	 */
	protected EList<JvmTypeReference> firedEvents;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected SarlActionImplCustom() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return SarlPackage.Literals.SARL_ACTION;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<JvmTypeReference> getFiredEvents() {
		if (this.firedEvents == null) {
			this.firedEvents = new EObjectContainmentEList<>(
					JvmTypeReference.class, this, SarlPackage.SARL_ACTION__FIRED_EVENTS);
		}
		return this.firedEvents;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
		case SarlPackage.SARL_ACTION__FIRED_EVENTS:
			return ((InternalEList<?>) getFiredEvents()).basicRemove(otherEnd, msgs);
		default:
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case SarlPackage.SARL_ACTION__FIRED_EVENTS:
			return getFiredEvents();
		default:
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
		case SarlPackage.SARL_ACTION__FIRED_EVENTS:
			getFiredEvents().clear();
			getFiredEvents().addAll((Collection<? extends JvmTypeReference>) newValue);
			return;
		default:
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
		case SarlPackage.SARL_ACTION__FIRED_EVENTS:
			getFiredEvents().clear();
			return;
		default:
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
		case SarlPackage.SARL_ACTION__FIRED_EVENTS:
			return this.firedEvents != null && !this.firedEvents.isEmpty();
		default:
		}
		return super.eIsSet(featureID);
	}

}
