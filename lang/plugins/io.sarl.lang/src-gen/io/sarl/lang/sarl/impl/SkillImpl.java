/**
 */
package io.sarl.lang.sarl.impl;

import io.sarl.lang.sarl.Capacity;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.Skill;
import io.sarl.lang.sarl.SkillFeature;

import java.util.Collection;

import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Skill</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.impl.SkillImpl#getImplementedCapacities <em>Implemented Capacities</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.SkillImpl#getFeatures <em>Features</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class SkillImpl extends AbstractElementImpl implements Skill
{
  /**
   * The cached value of the '{@link #getImplementedCapacities() <em>Implemented Capacities</em>}' reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getImplementedCapacities()
   * @generated
   * @ordered
   */
  protected EList<Capacity> implementedCapacities;

  /**
   * The cached value of the '{@link #getFeatures() <em>Features</em>}' containment reference list.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getFeatures()
   * @generated
   * @ordered
   */
  protected EList<SkillFeature> features;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected SkillImpl()
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
    return SarlPackage.Literals.SKILL;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<Capacity> getImplementedCapacities()
  {
    if (implementedCapacities == null)
    {
      implementedCapacities = new EObjectResolvingEList<Capacity>(Capacity.class, this, SarlPackage.SKILL__IMPLEMENTED_CAPACITIES);
    }
    return implementedCapacities;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public EList<SkillFeature> getFeatures()
  {
    if (features == null)
    {
      features = new EObjectContainmentEList<SkillFeature>(SkillFeature.class, this, SarlPackage.SKILL__FEATURES);
    }
    return features;
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
      case SarlPackage.SKILL__FEATURES:
        return ((InternalEList<?>)getFeatures()).basicRemove(otherEnd, msgs);
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
      case SarlPackage.SKILL__IMPLEMENTED_CAPACITIES:
        return getImplementedCapacities();
      case SarlPackage.SKILL__FEATURES:
        return getFeatures();
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
      case SarlPackage.SKILL__IMPLEMENTED_CAPACITIES:
        getImplementedCapacities().clear();
        getImplementedCapacities().addAll((Collection<? extends Capacity>)newValue);
        return;
      case SarlPackage.SKILL__FEATURES:
        getFeatures().clear();
        getFeatures().addAll((Collection<? extends SkillFeature>)newValue);
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
      case SarlPackage.SKILL__IMPLEMENTED_CAPACITIES:
        getImplementedCapacities().clear();
        return;
      case SarlPackage.SKILL__FEATURES:
        getFeatures().clear();
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
      case SarlPackage.SKILL__IMPLEMENTED_CAPACITIES:
        return implementedCapacities != null && !implementedCapacities.isEmpty();
      case SarlPackage.SKILL__FEATURES:
        return features != null && !features.isEmpty();
    }
    return super.eIsSet(featureID);
  }

} //SkillImpl
