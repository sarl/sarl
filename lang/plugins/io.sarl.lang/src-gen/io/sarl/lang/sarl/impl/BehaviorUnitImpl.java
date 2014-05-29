/**
 */
package io.sarl.lang.sarl.impl;

import io.sarl.lang.sarl.BehaviorUnit;
import io.sarl.lang.sarl.SarlPackage;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;

import org.eclipse.xtext.xbase.XExpression;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Behavior Unit</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link io.sarl.lang.sarl.impl.BehaviorUnitImpl#getEvent <em>Event</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.BehaviorUnitImpl#getGuard <em>Guard</em>}</li>
 *   <li>{@link io.sarl.lang.sarl.impl.BehaviorUnitImpl#getBody <em>Body</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class BehaviorUnitImpl extends FeatureImpl implements BehaviorUnit
{
  /**
   * The cached value of the '{@link #getEvent() <em>Event</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getEvent()
   * @generated
   * @ordered
   */
  protected JvmParameterizedTypeReference event;

  /**
   * The cached value of the '{@link #getGuard() <em>Guard</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getGuard()
   * @generated
   * @ordered
   */
  protected XExpression guard;

  /**
   * The cached value of the '{@link #getBody() <em>Body</em>}' containment reference.
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @see #getBody()
   * @generated
   * @ordered
   */
  protected XExpression body;

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  protected BehaviorUnitImpl()
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
    return SarlPackage.Literals.BEHAVIOR_UNIT;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public JvmParameterizedTypeReference getEvent()
  {
    return event;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetEvent(JvmParameterizedTypeReference newEvent, NotificationChain msgs)
  {
    JvmParameterizedTypeReference oldEvent = event;
    event = newEvent;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SarlPackage.BEHAVIOR_UNIT__EVENT, oldEvent, newEvent);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setEvent(JvmParameterizedTypeReference newEvent)
  {
    if (newEvent != event)
    {
      NotificationChain msgs = null;
      if (event != null)
        msgs = ((InternalEObject)event).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SarlPackage.BEHAVIOR_UNIT__EVENT, null, msgs);
      if (newEvent != null)
        msgs = ((InternalEObject)newEvent).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SarlPackage.BEHAVIOR_UNIT__EVENT, null, msgs);
      msgs = basicSetEvent(newEvent, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.BEHAVIOR_UNIT__EVENT, newEvent, newEvent));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public XExpression getGuard()
  {
    return guard;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetGuard(XExpression newGuard, NotificationChain msgs)
  {
    XExpression oldGuard = guard;
    guard = newGuard;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SarlPackage.BEHAVIOR_UNIT__GUARD, oldGuard, newGuard);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setGuard(XExpression newGuard)
  {
    if (newGuard != guard)
    {
      NotificationChain msgs = null;
      if (guard != null)
        msgs = ((InternalEObject)guard).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SarlPackage.BEHAVIOR_UNIT__GUARD, null, msgs);
      if (newGuard != null)
        msgs = ((InternalEObject)newGuard).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SarlPackage.BEHAVIOR_UNIT__GUARD, null, msgs);
      msgs = basicSetGuard(newGuard, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.BEHAVIOR_UNIT__GUARD, newGuard, newGuard));
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public XExpression getBody()
  {
    return body;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public NotificationChain basicSetBody(XExpression newBody, NotificationChain msgs)
  {
    XExpression oldBody = body;
    body = newBody;
    if (eNotificationRequired())
    {
      ENotificationImpl notification = new ENotificationImpl(this, Notification.SET, SarlPackage.BEHAVIOR_UNIT__BODY, oldBody, newBody);
      if (msgs == null) msgs = notification; else msgs.add(notification);
    }
    return msgs;
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  public void setBody(XExpression newBody)
  {
    if (newBody != body)
    {
      NotificationChain msgs = null;
      if (body != null)
        msgs = ((InternalEObject)body).eInverseRemove(this, EOPPOSITE_FEATURE_BASE - SarlPackage.BEHAVIOR_UNIT__BODY, null, msgs);
      if (newBody != null)
        msgs = ((InternalEObject)newBody).eInverseAdd(this, EOPPOSITE_FEATURE_BASE - SarlPackage.BEHAVIOR_UNIT__BODY, null, msgs);
      msgs = basicSetBody(newBody, msgs);
      if (msgs != null) msgs.dispatch();
    }
    else if (eNotificationRequired())
      eNotify(new ENotificationImpl(this, Notification.SET, SarlPackage.BEHAVIOR_UNIT__BODY, newBody, newBody));
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
      case SarlPackage.BEHAVIOR_UNIT__EVENT:
        return basicSetEvent(null, msgs);
      case SarlPackage.BEHAVIOR_UNIT__GUARD:
        return basicSetGuard(null, msgs);
      case SarlPackage.BEHAVIOR_UNIT__BODY:
        return basicSetBody(null, msgs);
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
      case SarlPackage.BEHAVIOR_UNIT__EVENT:
        return getEvent();
      case SarlPackage.BEHAVIOR_UNIT__GUARD:
        return getGuard();
      case SarlPackage.BEHAVIOR_UNIT__BODY:
        return getBody();
    }
    return super.eGet(featureID, resolve, coreType);
  }

  /**
   * <!-- begin-user-doc -->
   * <!-- end-user-doc -->
   * @generated
   */
  @Override
  public void eSet(int featureID, Object newValue)
  {
    switch (featureID)
    {
      case SarlPackage.BEHAVIOR_UNIT__EVENT:
        setEvent((JvmParameterizedTypeReference)newValue);
        return;
      case SarlPackage.BEHAVIOR_UNIT__GUARD:
        setGuard((XExpression)newValue);
        return;
      case SarlPackage.BEHAVIOR_UNIT__BODY:
        setBody((XExpression)newValue);
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
      case SarlPackage.BEHAVIOR_UNIT__EVENT:
        setEvent((JvmParameterizedTypeReference)null);
        return;
      case SarlPackage.BEHAVIOR_UNIT__GUARD:
        setGuard((XExpression)null);
        return;
      case SarlPackage.BEHAVIOR_UNIT__BODY:
        setBody((XExpression)null);
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
      case SarlPackage.BEHAVIOR_UNIT__EVENT:
        return event != null;
      case SarlPackage.BEHAVIOR_UNIT__GUARD:
        return guard != null;
      case SarlPackage.BEHAVIOR_UNIT__BODY:
        return body != null;
    }
    return super.eIsSet(featureID);
  }

} //BehaviorUnitImpl
