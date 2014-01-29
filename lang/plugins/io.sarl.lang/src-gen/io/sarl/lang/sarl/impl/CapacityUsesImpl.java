/**
 */
package io.sarl.lang.sarl.impl;

import io.sarl.lang.sarl.Capacity;
import io.sarl.lang.sarl.CapacityUses;
import io.sarl.lang.sarl.SarlPackage;

import java.util.Collection;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.util.EObjectResolvingEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Capacity Uses</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.impl.CapacityUsesImpl#getCapacitiesUsed <em>Capacities Used</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class CapacityUsesImpl extends AgentFeatureImpl implements CapacityUses
{
  /**
   * The cached value of the '{@link #getCapacitiesUsed() <em>Capacities Used</em>}' reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getCapacitiesUsed()
   * @generated
   * @ordered
   */
  protected EList<Capacity> capacitiesUsed;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected CapacityUsesImpl()
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
    return SarlPackage.Literals.CAPACITY_USES;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<Capacity> getCapacitiesUsed()
  {
    if (capacitiesUsed == null)
    {
      capacitiesUsed = new EObjectResolvingEList<Capacity>(Capacity.class, this, SarlPackage.CAPACITY_USES__CAPACITIES_USED);
    }
    return capacitiesUsed;
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
      case SarlPackage.CAPACITY_USES__CAPACITIES_USED:
        return getCapacitiesUsed();
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
      case SarlPackage.CAPACITY_USES__CAPACITIES_USED:
        getCapacitiesUsed().clear();
        getCapacitiesUsed().addAll((Collection<? extends Capacity>)newValue);
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
      case SarlPackage.CAPACITY_USES__CAPACITIES_USED:
        getCapacitiesUsed().clear();
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
      case SarlPackage.CAPACITY_USES__CAPACITIES_USED:
        return capacitiesUsed != null && !capacitiesUsed.isEmpty();
    }
    return super.eIsSet(featureID);
  }

} //CapacityUsesImpl
