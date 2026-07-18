from otree.api import *
import time
import random

doc = """
Multi-attribute continuous double auction with dynamic marginal costs and values, multiple rounds, and role randomisation. Includes stacking and bundling treatments with either a single package or a menu.
"""

class C(BaseConstants):
    NAME_IN_URL = 'bundle_auction'
    PLAYERS_PER_GROUP = None
    NUM_ROUNDS = 4
    
    ALL_PRODUCTS = ['Product A', 'Product B', 'Package', 'Package 1', 'Package 2']    

    # Heterogeneous cost parameters (alpha, gamma)
    PARAMETERS = [
        (0.32, -0.63),
        (0.32, -0.38),
        (0.61, -0.63),
        (0.61, -0.38)
    ]
    REPETITIONS = int(NUM_ROUNDS / 4)

    # Fixed benefit parameters
    OMEGA = 15.0
    THETA = 0.5

class Subsession(BaseSubsession):
    alpha = models.FloatField()
    gamma = models.FloatField()
    x_param = models.FloatField()

def creating_session(subsession: Subsession):
    if subsession.round_number == 1:
        full_sequence = C.PARAMETERS * C.REPETITIONS
        random.shuffle(full_sequence)
        subsession.session.vars['parameter_sequence'] = full_sequence
        
    current_alpha, current_gamma = subsession.session.vars['parameter_sequence'][subsession.round_number - 1]
    subsession.alpha = current_alpha
    subsession.gamma = current_gamma
    
    numerator = 1 - (current_alpha * current_gamma)
    denominator = (current_alpha**2) - (current_alpha * current_gamma)
    subsession.x_param = float(round(numerator / denominator))

    for group in subsession.get_groups():
        players = group.get_players()
        
        midpoint = len(players) // 2
        buyer_count = 1
        seller_count = 0
        
        for i, player in enumerate(players):
            if i < midpoint:
                player.is_buyer = True
                player.buyer_id = buyer_count
                player.seller_type = ""
                buyer_count += 1
            else:
                player.is_buyer = False
                player.buyer_id = 0
                if seller_count % 2 == 0:
                    player.seller_type = 'A'
                else:
                    player.seller_type = 'B'
                seller_count += 1

class Group(BaseGroup):
    pass

class Player(BasePlayer):
    is_buyer = models.BooleanField()
    buyer_id = models.IntegerField(initial=1)
    seller_type = models.StringField(blank=True)
    profit = models.FloatField(initial=0.0)

    trades_A = models.IntegerField(initial=0)
    trades_B = models.IntegerField(initial=0)
    trades_Pkg = models.IntegerField(initial=0)
    trades_Pkg1 = models.IntegerField(initial=0)
    trades_Pkg2 = models.IntegerField(initial=0)

    @property
    def underlying_qa(self):
        x = int(self.subsession.x_param)
        return self.trades_A + self.trades_Pkg + (self.trades_Pkg1 * 1.0) + (self.trades_Pkg2 * (1.0 / x))

    @property
    def underlying_qb(self):
        x = int(self.subsession.x_param)
        return self.trades_B + self.trades_Pkg + (self.trades_Pkg1 * (1.0 / x)) + (self.trades_Pkg2 * 1.0)

    @property
    def displayed_qa(self):
        x = int(self.subsession.x_param)
        return self.trades_A + self.trades_Pkg + (self.trades_Pkg1 * x) + (self.trades_Pkg2 * 1)

    @property
    def displayed_qb(self):
        x = int(self.subsession.x_param)
        return self.trades_B + self.trades_Pkg + (self.trades_Pkg1 * 1) + (self.trades_Pkg2 * x)

    def get_tc(self, qa, qb):
        if self.is_buyer: return 0.0
        
        a = self.subsession.alpha
        g = self.subsession.gamma

        if self.seller_type == 'A':
            cost_a = 0.5 * a * qa**2
            cost_b = 0.5 * (1.0 / a) * qb**2
        else:
            cost_a = 0.5 * (1.0 / a) * qa**2
            cost_b = 0.5 * a * qb**2
            
        interaction = g * qa * qb
        return cost_a + cost_b + interaction

    def get_tv(self, qa, qb, n_buyers):
        if not self.is_buyer: return 0.0
        
        def value_sum(q):
            if q <= 0: return 0.0
            
            m = int(q)
            f = q % 1
            
            base_mv = C.OMEGA - C.THETA * self.buyer_id
            val_int = m * base_mv - C.THETA * n_buyers * (m * (m - 1) / 2.0)            
            val_frac = f * (base_mv - C.THETA * n_buyers * m)
            
            return val_int + val_frac
            
        return value_sum(qa) + value_sum(qb)

    def evaluate_marginal_change(self, p_type, n_buyers):
        qa = self.underlying_qa
        qb = self.underlying_qb
        x = self.subsession.x_param

        if p_type == 'Product A': delta_a, delta_b = 1.0, 0.0
        elif p_type == 'Product B': delta_a, delta_b = 0.0, 1.0
        elif p_type == 'Package': delta_a, delta_b = 1.0, 1.0
        elif p_type == 'Package 1': delta_a, delta_b = 1.0, (1.0 / x)
        elif p_type == 'Package 2': delta_a, delta_b = (1.0 / x), 1.0
        else: return 0.0
        
        if self.is_buyer:
            return self.get_tv(qa + delta_a, qb + delta_b, n_buyers) - self.get_tv(qa, qb, n_buyers)
        else:
            return self.get_tc(qa + delta_a, qb + delta_b) - self.get_tc(qa, qb)

class Order(ExtraModel):
    group = models.Link(Group)
    player = models.Link(Player)
    is_bid = models.BooleanField()
    product_type = models.StringField()
    price = models.IntegerField()
    timestamp = models.FloatField()
    is_active = models.BooleanField(initial=True) 

class Trade(ExtraModel):
    group = models.Link(Group)
    buyer = models.Link(Player)
    seller = models.Link(Player)
    product_type = models.StringField()
    price = models.IntegerField()
    buyer_profit = models.FloatField()
    seller_profit = models.FloatField()
    timestamp = models.FloatField()

class Welcome(Page):
    @staticmethod
    def is_displayed(player):
        return player.round_number == 1

    @staticmethod
    def vars_for_template(player):
        treatment = player.session.config.get('treatment', 'baseline')
        x_weight = int(player.subsession.x_param) 
        
        return {
            'treatment': treatment,
            'x_weight': x_weight
        }

class ReadyToStart(WaitPage):
    pass

class Trading(Page):
    timeout_seconds = 180

    @staticmethod
    def vars_for_template(player):
        treatment = player.session.config.get('treatment', 'baseline')
        x = int(player.subsession.x_param)

        if treatment == 'single_package':
            active_products = [{'id': 'Package', 'safe_id': 'package', 'label': 'Package (1x Product A, 1x Product B)'}]
        elif treatment == 'package_menu':
            active_products = [
                {'id': 'Package 1', 'safe_id': 'package-1', 'label': f'Package 1 ({x}x Product A, 1x Product B)'},
                {'id': 'Package 2', 'safe_id': 'package-2', 'label': f'Package 2 (1x Product A, {x}x Product B)'}
            ]
        else:
            active_products = [
                {'id': 'Product A', 'safe_id': 'product-a', 'label': 'Product A'},
                {'id': 'Product B', 'safe_id': 'product-b', 'label': 'Product B'}
            ]
            
        return {'active_products': active_products, 'treatment': treatment}
    
    @staticmethod
    def js_vars(player):
        treatment = player.session.config.get('treatment', 'baseline')
        
        if treatment == 'single_package':
            active_products = [{'id': 'Package', 'safe_id': 'package', 'label': 'Package'}]
        elif treatment == 'package_menu':
            active_products = [
                {'id': 'Package 1', 'safe_id': 'package-1', 'label': 'Package 1'},
                {'id': 'Package 2', 'safe_id': 'package-2', 'label': 'Package 2'}
            ]
        else:
            active_products = [
                {'id': 'Product A', 'safe_id': 'product-a', 'label': 'Product A'},
                {'id': 'Product B', 'safe_id': 'product-b', 'label': 'Product B'}
            ]
            
        return dict(active_products=active_products, is_buyer=player.is_buyer)
    
    @staticmethod
    def live_method(player, data):
        group = player.group
        players = group.get_players()
        n_buyers = sum([1 for p in players if p.is_buyer])

        treatment = player.session.config.get('treatment', 'baseline')
        
        if treatment == 'single_package': valid_products = ['Package']
        elif treatment == 'package_menu': valid_products = ['Package 1', 'Package 2']
        else: valid_products = ['Product A', 'Product B']

        if 'action' in data:
            if data['action'] in ['bid', 'ask']:
                is_bid = (data['action'] == 'bid')
                    
                if is_bid == player.is_buyer:
                    new_price = int(data['price'])
                    p_type = data['product_type']
                    
                    if is_bid:
                        current_bids = Order.filter(group=group, product_type=p_type, is_bid=True, is_active=True)
                        if current_bids:
                            max_bid = max([b.price for b in current_bids])
                            if new_price <= max_bid:
                                return {player.id_in_group: {'error': f'Invalid Order: Your bid for {p_type} must be greater than the current maximum bid of ${max_bid}.'}}
                    else:
                        current_asks = Order.filter(group=group, product_type=p_type, is_bid=False, is_active=True)
                        if current_asks:
                            min_ask = min([a.price for a in current_asks])
                            if new_price >= min_ask:
                                return {player.id_in_group: {'error': f'Invalid Order: Your ask for {p_type} must be lower than the current minimum ask of ${min_ask}.'}}

                    previous_orders = Order.filter(group=group, player=player, product_type=p_type, is_active=True)
                    for po in previous_orders:
                        po.is_active = False

                    new_order = Order.create(
                        group=group, player=player, is_bid=is_bid, product_type=p_type,
                        price=new_price, timestamp=time.time(), is_active=True
                    )
                    
                    opposite_is_bid = not new_order.is_bid
                    candidates = Order.filter(group=group, is_active=True, is_bid=opposite_is_bid, product_type=new_order.product_type)
                    valid_candidates = [c for c in candidates if c.player != player]
                    
                    if new_order.is_bid:
                        matches = [c for c in valid_candidates if c.price <= new_order.price]
                        matches.sort(key=lambda x: (x.price, x.timestamp))
                    else:
                        matches = [c for c in valid_candidates if c.price >= new_order.price]
                        matches.sort(key=lambda x: (-x.price, x.timestamp))
                        
                    if matches:
                        match = matches[0] 
                        trade_price = match.price
                        
                        new_order.is_active = False
                        match.is_active = False
                            
                        buyer = new_order.player if new_order.is_bid else match.player
                        seller = match.player if new_order.is_bid else new_order.player
                        
                        b_profit = buyer.evaluate_marginal_change(p_type, n_buyers) - trade_price
                        s_profit = trade_price - seller.evaluate_marginal_change(p_type, n_buyers)
                        
                        buyer.profit += b_profit
                        seller.profit += s_profit
                        
                        for p_obj in [buyer, seller]:
                            if p_type == 'Product A': p_obj.trades_A += 1
                            elif p_type == 'Product B': p_obj.trades_B += 1
                            elif p_type == 'Package': p_obj.trades_Pkg += 1
                            elif p_type == 'Package 1': p_obj.trades_Pkg1 += 1
                            elif p_type == 'Package 2': p_obj.trades_Pkg2 += 1                        
                        
                        Trade.create(
                            group=group, buyer=buyer, seller=seller, product_type=p_type, 
                            price=trade_price, buyer_profit=b_profit, seller_profit=s_profit, timestamp=time.time()
                        )

        active_orders = Order.filter(group=group, is_active=True)
        trades = Trade.filter(group=group)
            
        response = {}
        for p in players:
            player_order_book = {ptype: {'bids': [], 'asks': []} for ptype in valid_products}
            
            for o in active_orders:
                if o.product_type in valid_products:
                    order_dict = {'price': o.price, 'is_mine': (o.player == p)}
                    if o.is_bid:
                        player_order_book[o.product_type]['bids'].append(order_dict)
                    else:
                        player_order_book[o.product_type]['asks'].append(order_dict)
                        
            for ptype in valid_products:
                player_order_book[ptype]['bids'].sort(key=lambda x: x['price'], reverse=True)
                player_order_book[ptype]['asks'].sort(key=lambda x: x['price'])
                
            player_trade_list = [{'type': t.product_type, 'price': t.price, 'is_mine': (t.buyer == p or t.seller == p)} for t in trades]
            
            marginals_dict = {}
            for prod in valid_products:
                marginals_dict[prod] = float(p.evaluate_marginal_change(prod, n_buyers))

            response[p.id_in_group] = {
                'order_book': player_order_book,
                'trades': player_trade_list,
                'inventory': {
                    'profit': float(p.profit),
                    'displayed_a': p.displayed_qa,
                    'displayed_b': p.displayed_qb,
                },
                'marginals': marginals_dict
            }
        return response

class BetweenRounds(Page):
    timeout_seconds = 30
    @staticmethod
    def is_displayed(player): return player.round_number < C.NUM_ROUNDS
    @staticmethod
    def vars_for_template(player): return {'total_profit': sum([p.profit for p in player.in_all_rounds()])}

class FinalResults(Page):
    @staticmethod
    def is_displayed(player): return player.round_number == C.NUM_ROUNDS
    @staticmethod
    def vars_for_template(player): return {'total_profit': sum([p.profit for p in player.in_all_rounds()])}

page_sequence = [ReadyToStart, Trading, BetweenRounds, FinalResults]