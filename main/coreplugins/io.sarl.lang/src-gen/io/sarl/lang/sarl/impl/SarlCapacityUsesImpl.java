/**
 */
package io.sarl.lang.sarl.impl;

import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlPackage;

import java.util.Collection;

import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

import org.eclipse.xtend.core.xtend.impl.XtendMemberImplCustom;

import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Capacity Uses</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * </p>
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.impl.SarlCapacityUsesImpl#getCapacities <em>Capacities</em>}</li>
 * </ul>
 *
 * @generated
 */
public class SarlCapacityUsesImpl extends XtendMemberImplCustom implements SarlCapacityUses
{
	/**
	 * The cached value of the '{@link #getCapacities() <em>Capacities</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getCapacities()
	 * @generated
	 * @ordered
	 */
	protected EList<JvmParameterizedTypeReference> capacities;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected SarlCapacityUsesImpl()
	{
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass()
	{
		return SarlPackage.Literals.SARL_CAPACITY_USES;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EList<JvmParameterizedTypeReference> getCapacities()
	{
		if (capacities == null)
		{
			capacities = new EObjectContainmentEList<JvmParameterizedTypeReference>(JvmParameterizedTypeReference.class, this, SarlPackage.SARL_CAPACITY_USES__CAPACITIES);
		}
		return capacities;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs)
	{
		switch (featureID)
		{
			case SarlPackage.SARL_CAPACITY_USES__CAPACITIES:
				return ((InternalEList<?>)getCapacities()).basicRemove(otherEnd, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType)
	{
		switch (featureID)
		{
			case SarlPackage.SARL_CAPACITY_USES__CAPACITIES:
				return getCapacities();
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
	public void eSet(int featureID, Object newValue)
	{
		switch (featureID)
		{
			case SarlPackage.SARL_CAPACITY_USES__CAPACITIES:
				getCapacities().clear();
				getCapacities().addAll((Collection<? extends JvmParameterizedTypeReference>)newValue);
				return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eUnset(int featureID)
	{
		switch (featureID)
		{
			case SarlPackage.SARL_CAPACITY_USES__CAPACITIES:
				getCapacities().clear();
				return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID)
	{
		switch (featureID)
		{
			case SarlPackage.SARL_CAPACITY_USES__CAPACITIES:
				return capacities != null && !capacities.isEmpty();
		}
		return super.eIsSet(featureID);
	}

} //SarlCapacityUsesImpl
